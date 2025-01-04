{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module CaseX
  ( pattern SPLIT
  , plugin
  ) where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Trans.Writer.CPS as Writer
import           Data.Foldable
import           Data.Functor
import qualified Data.Generics as Syb
import           Data.IORef
import           Data.Maybe
import           Data.Monoid (Any(..))
import qualified Data.Map.Strict as M
import           Data.String (IsString, fromString)
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified CaseX.GhcFacade as Ghc

-- | Used to induce the incomplete patterns warning from GHC
pattern SPLIT :: a
pattern SPLIT <- _

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = \_ hscEnv -> pure . turnOnIncompletePatternWarn $ addDsHook hscEnv
  , Ghc.parsedResultAction = \_ _ result -> pure $ addImport result
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = \_ env groups ->
      removeUnusedImportWarn >> pure (env, groups)
  , Ghc.typeCheckResultAction = \_ _ env ->
      removeUnusedImportWarn >> pure env
  }

-- | Plugin only functions if incomplete patterns warning is enabled, so we force it on.
turnOnIncompletePatternWarn :: Ghc.HscEnv -> Ghc.HscEnv
turnOnIncompletePatternWarn hscEnv = hscEnv
  { Ghc.hsc_dflags =
      let dflags = Ghc.hsc_dflags hscEnv
       in dflags
          { Ghc.warningFlags = EnumSet.insert Ghc.Opt_WarnIncompletePatterns $ Ghc.warningFlags dflags }
  }

-- | Incomplete patterns warning is emitted by the desugarer. Some shenanigans
-- are needed to intercept these warnings: a custom error is added to the
-- frontend result which causes the desugarer to throw a 'SourceError'
-- exception containing all diagnostics after running. This exception is then
-- caught (and later rethrown) by the plugin.
addDsHook :: Ghc.HscEnv -> Ghc.HscEnv
addDsHook hscEnv = hscEnv
  { Ghc.hsc_hooks =
      let hooks = Ghc.hsc_hooks hscEnv
       in hooks
          { Ghc.runPhaseHook = Just $ phaseHook (Ghc.runPhaseHook hooks) }
  }
  where
    phaseHook mExistingHook = Ghc.PhaseHook $ \tPhase -> do
      let isMissingPatWarn msgEnv =
            case Ghc.errMsgDiagnostic msgEnv of
              Ghc.GhcDsMessage (Ghc.DsNonExhaustivePatterns _ _ _ patIds nablas)
                | Just srcSpan <- Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan msgEnv) -> Left
                NonExhaustivePattern
                  { patternIds = patIds
                  , patternNablas = nablas
                  , srcCodeLoc = srcSpan
                  }
              _ -> Right msgEnv
          runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h) mExistingHook
      case tPhase of
        Ghc.T_HscPostTc env modSum tcResult@(Ghc.FrontendTypecheck gblEnv) warns mOldHash -> do
          usedGres <- readIORef $ Ghc.tcg_used_gres gblEnv
          let usesSplit = any ((== splitName) . Ghc.getOccFS . Ghc.gre_name) usedGres
              mFilePath = Ghc.ml_hs_file (Ghc.ms_location modSum)
          if not usesSplit
          then runPhaseOrExistingHook tPhase
          else do
            let customError =
                  Ghc.mkPlainErrorMsgEnvelope
                    (maybe Ghc.noSrcSpan (Ghc.mkGeneralSrcSpan . fromString) mFilePath)
                    (Ghc.ghcUnknownMessage PatternSplitDiag)
                warnsWithError = Ghc.addMessage customError warns
            catch
              (runPhaseOrExistingHook $ Ghc.T_HscPostTc env modSum tcResult warnsWithError mOldHash)
              (\(Ghc.SourceError msgs) -> do
                let (missingPatWarns, otherWarns) = Ghc.partitionBagWith isMissingPatWarn (Ghc.getMessages msgs)
                mResult <- EP.parseModule Paths.libdir `traverse` mFilePath
                case mResult of
                  Just (Right parsedMod) -> do
                    let qualiImps = getQualifiedImports parsedMod
                    case splitPattern qualiImps missingPatWarns parsedMod of
                      (ps, Any True) ->
                        traverse_ (flip writeFile $ EP.exactPrint ps) mFilePath
                      _ -> pure ()
                  _ -> pure ()
                throw . Ghc.SourceError $ Ghc.mkMessages otherWarns
              )
        _ -> runPhaseOrExistingHook tPhase

-- | Diagnostic thrown when case splitting should be attempted.
data PatternSplitDiag = PatternSplitDiag

instance Ghc.Diagnostic PatternSplitDiag where
  type DiagnosticOpts PatternSplitDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated (Ghc.text "Splitting targeted patterns (this is not an error)")
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing

data NonExhaustivePattern = NonExhaustivePattern
  { patternIds :: [Ghc.Id]
  , patternNablas :: [Ghc.Nabla]
  , srcCodeLoc :: Ghc.RealSrcSpan
  }

-- | Finds the target pattern and splits it. Returns the modified source and True if successful.
splitPattern
  :: M.Map Ghc.ModuleName Ghc.ModuleName
  -> Ghc.Bag NonExhaustivePattern
  -> Ghc.ParsedSource
  -> (Ghc.ParsedSource, Any)
splitPattern qualiImps nePats ps =
    Writer.runWriter $
      Syb.everywhereM
        ( Syb.mkM (Writer.writer . goExpr)
          `Syb.extM` (Writer.writer . goDecl)
        ) ps
  where
  goExpr :: Ghc.LHsExpr Ghc.GhcPs -> (Ghc.LHsExpr Ghc.GhcPs, Any)
  goExpr expr@(Ghc.L ann (Ghc.HsCase x scrut _))
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just nePat <- find ((caseLoc ==) . srcCodeLoc) nePats
    , Ghc.L _ (Ghc.HsCase _ _ deltaMG@(Ghc.MG _ (Ghc.L _ _matches)))
        <- EP.makeDeltaAst expr
    , Just newMG <- splitMG False False 0 nePat deltaMG
    = ( Ghc.L ann (Ghc.HsCase x scrut newMG)
      , Any True
      )
  goExpr expr@(Ghc.L ann (Ghc.HsLam x lamType _))
    | lamType /= Ghc.LamSingle
    , Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just nePat <- find ((caseLoc ==) . srcCodeLoc) nePats
    , Ghc.L _ (Ghc.HsLam _ _ deltaMG@(Ghc.MG _ (Ghc.L matchesAnn _matches)))
        <- EP.makeDeltaAst expr
    , Just newMG <- splitMG True False (colDelta matchesAnn) nePat deltaMG
    = ( Ghc.L ann (Ghc.HsLam x lamType newMG)
      , Any True
      )
  goExpr expr = (expr, Any False)

  goDecl :: Ghc.LHsDecl Ghc.GhcPs -> (Ghc.LHsDecl Ghc.GhcPs, Any)
  goDecl decl@(Ghc.L ann (Ghc.ValD x (Ghc.FunBind x2 fid _)))
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just nePat <- find ((caseLoc ==) . srcCodeLoc) nePats
    , Ghc.L _ (Ghc.ValD _ (Ghc.FunBind _ _ deltaMG@(Ghc.MG _ (Ghc.L _ _))))
        <- EP.makeDeltaAst decl
    , Just newMG <- splitMG True True 0 nePat deltaMG
    = ( Ghc.L ann (Ghc.ValD x (Ghc.FunBind x2 fid newMG))
      , Any True
      )
  goDecl decl = (decl, Any False)

  -- Should be applied after the delta transformation
  splitMG multiplePats offsetFirstPat colOffset nePat (Ghc.MG x2 (Ghc.L ann2 matches))
    | (beforeSplit, Ghc.L splitAnn (Ghc.Match _ ctx _ rhs) : afterSplit)
        <- break matchHasSplit matches
    , let -- If splitting the first match, trailing matches need to have a delta
          -- putting it on a new line
          correctDeltas [] = []
          correctDeltas (x : xs) =
            x :
              (Ghc.L (Ghc.EpAnn (Ghc.EpaDelta (Ghc.DifferentLine 1 colOffset) []) Ghc.noAnn Ghc.emptyComments) . Ghc.unLoc
               <$> xs)
          newMatches = correctDeltas $ do
            nabla <- patternNablas nePat
            let pats =
                  zipWith (mkPat qualiImps nabla $ not multiplePats)
                          (offsetFirstPat : repeat True)
                          (patternIds nePat)
            [ Ghc.L splitAnn $ Ghc.Match [] ctx pats rhs ]
          newMatchGroup = beforeSplit ++ newMatches ++ filter (not . matchHasSplit) afterSplit
          -- The same comment can appear in both the matches annotation and the
          -- annotation of individual matches. Remove from the outer annotation
          -- if so.
          removeDupeComments (Ghc.EpAnn d a (Ghc.EpaComments comments)) =
            let isDupe (Ghc.L _ comment) = or $ do
                  Ghc.L (Ghc.EpAnn _ _ (Ghc.EpaComments coms)) _ <- matches
                  Ghc.L _ com <- coms
                  [ Ghc.ac_prior_tok comment == Ghc.ac_prior_tok com ]

                newComments = filter (not . isDupe) comments
             in Ghc.EpAnn d a (Ghc.EpaComments newComments)
          removeDupeComments x = x
    = Just $ Ghc.MG x2 (Ghc.L (removeDupeComments ann2) newMatchGroup)
    | otherwise = Nothing

colDelta :: Ghc.EpAnn ann -> Int
colDelta (Ghc.EpAnn (Ghc.EpaDelta delta _) _ _) = case delta of
  Ghc.DifferentLine _ c -> c
  Ghc.SameLine c -> c
colDelta _ = 0

matchHasSplit :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Bool
matchHasSplit (Ghc.L _ (Ghc.Match _ _ pats _)) = Syb.everything (||) (Syb.mkQ False isSplitCon) pats
  where
  isSplitCon :: Ghc.Pat Ghc.GhcPs -> Bool
  isSplitCon (Ghc.ConPat [] (Ghc.L _ rdr) _) =
    Ghc.rdrNameOcc rdr == Ghc.mkDataOcc splitName
  isSplitCon _ = False

-- | Produce a 'Pat' for a missing pattern
mkPat
  :: M.Map Ghc.ModuleName Ghc.ModuleName
  -> Ghc.Nabla
  -> Bool -- ^ True => is a singular pattern which doesn't need outer parens
  -> Bool -- ^ True => needs left padding due separate it from another pattern
  -> Ghc.Id
  -> Ghc.LPat Ghc.GhcPs
mkPat qualiImps nabla isOutermost needsLeftPad x = Ghc.L (Ghc.EpAnn delta Ghc.noAnn Ghc.emptyComments) $
  case Ghc.lookupSolution nabla x of
    Nothing -> Ghc.WildPat Ghc.noExtField
    Just (Ghc.PACA (Ghc.PmAltConLike con) _tvs args) -> paren args $
      -- TODO use literal patterns for lists, tuples, etc
      case Ghc.conLikeIsInfix con of
        True | [arg1, arg2] <- args ->
          Ghc.ConPat []
            ( Ghc.L (Ghc.EpAnn EP.d1 Ghc.noAnn Ghc.emptyComments)
            . nameToRdrName qualiImps
            $ Ghc.conLikeName con
            )
          $ Ghc.InfixCon (mkPat qualiImps nabla False False arg1)
                         (mkPat qualiImps nabla False True arg2)
        _ ->
          -- If GHC tries to use SPLIT as a missing pattern, replace it with wildcard
          if Ghc.occName (Ghc.conLikeName con) == Ghc.mkDataOcc splitName
          then Ghc.WildPat Ghc.noExtField
          else Ghc.ConPat [] ( Ghc.L (Ghc.EpAnn EP.d0 Ghc.noAnn Ghc.emptyComments)
                             . nameToRdrName qualiImps $ Ghc.conLikeName con
                             )
             $ Ghc.PrefixCon [] (mkPat qualiImps nabla False True <$> args)
    Just (Ghc.PACA (Ghc.PmAltLit lit) _tvs _args) -> Ghc.LitPat Ghc.noExtField $
      case Ghc.pm_lit_val lit of
        Ghc.PmLitInt integer -> Ghc.HsInteger Ghc.NoSourceText integer (Ghc.pmLitType lit)
        Ghc.PmLitRat rational -> Ghc.HsRat Ghc.noExtField (Ghc.fractionalLitFromRational rational) (Ghc.pmLitType lit)
        Ghc.PmLitChar char -> Ghc.HsChar Ghc.NoSourceText char
        Ghc.PmLitString fastString -> Ghc.HsString Ghc.NoSourceText fastString
        Ghc.PmLitOverInt minuses integer ->
          Ghc.HsInteger Ghc.NoSourceText
            (if odd minuses then negate integer else integer)
            (Ghc.pmLitType lit)
        Ghc.PmLitOverRat minuses fractionalLit ->
          Ghc.HsRat Ghc.noExtField
            ( Ghc.fractionalLitFromRational
            . (if odd minuses then negate else id)
            $ Ghc.rationalFromFractionalLit fractionalLit
            )
            (Ghc.pmLitType lit)
        Ghc.PmLitOverString fastString -> Ghc.HsString Ghc.NoSourceText fastString
  where
    delta = if needsLeftPad then EP.d1 else EP.d0
    paren [] inner = inner
    paren _ inner =
      if not isOutermost
      then Ghc.ParPat (Ghc.EpTok EP.d0, Ghc.EpTok EP.d0) (Ghc.L (Ghc.EpAnn EP.d0 Ghc.noAnn Ghc.emptyComments) inner)
      else inner

-- | Adds the import for SPLIT to the module being compiled. Otherwise users
-- would have to manually add this import everytime they want to do pattern splitting.
addImport :: Ghc.ParsedResult -> Ghc.ParsedResult
addImport result = result
  { Ghc.parsedResultModule =
    let resMod = Ghc.parsedResultModule result
     in resMod
        { Ghc.hpm_module = Ghc.hpm_module resMod <&> \pMod ->
            pMod
              { Ghc.hsmodImports =
                  caseXImport : Ghc.hsmodImports pMod
              }
        }
  }
  where
    caseXImport = Ghc.noLocA . Ghc.simpleImportDecl $ Ghc.mkModuleName caseXName

-- | The automatically added import gets flagged as unused even if it is used.
-- The solution here is to simply suppress the warning.
removeUnusedImportWarn :: Ghc.TcM ()
removeUnusedImportWarn = do
  errsVar <- Ghc.getErrsVar
  let isCaseXImportWarn msgEnv =
        case Ghc.errMsgDiagnostic msgEnv of
          Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnUnusedImport decl _)) ->
            Ghc.unLoc (Ghc.ideclName decl) == Ghc.mkModuleName caseXName
          _ -> False
  Ghc.liftIO . modifyIORef errsVar $
    Ghc.mkMessages . Ghc.filterBag (not . isCaseXImportWarn) . Ghc.getMessages

-- | It is necessary to track what modules are imported qualified so that
-- constructors in generated patterns can be correctly qualified.
getQualifiedImports :: Ghc.ParsedSource -> M.Map Ghc.ModuleName Ghc.ModuleName
getQualifiedImports (Ghc.L _ mo) = M.fromList . mapMaybe getQuali $ Ghc.hsmodImports mo
  where
    getQuali (Ghc.L _ x) = do
      guard $ Ghc.ideclQualified x /= Ghc.NotQualified
      let mQualAs = Ghc.unLoc <$> Ghc.ideclAs x
          modName = Ghc.unLoc $ Ghc.ideclName x
      Just (modName, fromMaybe modName mQualAs)

nameToRdrName :: M.Map Ghc.ModuleName Ghc.ModuleName -> Ghc.Name -> Ghc.RdrName
nameToRdrName qualiImps n =
  case M.lookup modName qualiImps of
    Nothing -> Ghc.nameRdrName n
    Just qual -> Ghc.mkRdrQual qual $ Ghc.getOccName n
  where
    modName = Ghc.moduleName $ Ghc.nameModule n

splitName :: IsString a => a
splitName = "SPLIT"

caseXName :: IsString a => a
caseXName = "CaseX"
