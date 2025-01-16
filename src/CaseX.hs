{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module CaseX
  ( pattern SPLIT
  , plugin
  ) where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.Char as Char
import           Data.Foldable
import           Data.Functor
import qualified Data.Generics as Syb
import           Data.IORef
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid (Any(..))
import qualified Data.Map.Strict as M
import           Data.String (IsString, fromString)
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import           Text.Read (readMaybe)

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
                    -- Need to parse the module because GHC removes
                    -- ParsedResult from ModSummary after the frontend finishes.
                    -- Use the options from compilation to parse the module, otherwise certain
                    -- syntax extensions won't parse correctly.
                    dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
                mResult <- EP.ghcWrapper Paths.libdir $
                  fmap EP.postParseTransform
                    <$> EP.parseModuleEpAnnsWithCppInternal EP.defaultCppOptions dynFlags
                          `traverse` mFilePath
                case mResult of
                  Just (Right parsedMod) -> do
                    let qualiImps = getQualifiedImports parsedMod
                        ast = EP.makeDeltaAst
                            $ searchAndMark (Ghc.bagToList missingPatWarns) parsedMod
                    case splitPattern qualiImps (Ghc.bagToList missingPatWarns) ast of
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

-- | Before applying delta transformation, find the expressions that go with
-- non exhaustive patterns and mark them with a special comment containing the
-- index of that pattern. This must be done first because source code locations
-- are removed by delta transformation.
-- Problematic if delta moves comments to a different node, hopefully it won't.
searchAndMark
  :: [NonExhaustivePattern]
  -> Ghc.ParsedSource
  -> Ghc.ParsedSource
searchAndMark nePats = Syb.everywhere (Syb.mkT goExpr `Syb.extT` goDecl)
  where
  goExpr :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
  goExpr (Ghc.L ann c@Ghc.HsCase{})
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just neIdx <- List.findIndex ((caseLoc ==) . srcCodeLoc) nePats
    = Ghc.L (addIndexComment ann neIdx) c
#if MIN_VERSION_ghc(9,10,0)
  goExpr (Ghc.L ann l@(Ghc.HsLam _ lamType _))
    | lamType /= Ghc.LamSingle
    , Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just neIdx <- List.findIndex ((caseLoc ==) . srcCodeLoc) nePats
    = Ghc.L (addIndexComment ann neIdx) l
#elif MIN_VERSION_ghc(9,8,0)
  goExpr (Ghc.L ann l@(Ghc.HsLamCase _ _ _))
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just neIdx <- List.findIndex ((caseLoc ==) . srcCodeLoc) nePats
    = Ghc.L (addIndexComment ann neIdx) l
#endif
  goExpr x = x

  goDecl :: Ghc.LHsDecl Ghc.GhcPs -> Ghc.LHsDecl Ghc.GhcPs
  goDecl (Ghc.L ann f@(Ghc.ValD _ Ghc.FunBind{}))
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just neIdx <- List.findIndex ((caseLoc ==) . srcCodeLoc) nePats
    = Ghc.L (addIndexComment ann neIdx) f
  goDecl x = x

  addIndexComment ann neIdx =
    let com :: Ghc.LEpaComment
        com = Ghc.L Ghc.fakeCommentLocation
          (Ghc.EpaComment (Ghc.EpaLineComment (show neIdx)) Ghc.placeholderRealSpan)
        newComms = case Ghc.getComments ann of
          Ghc.EpaComments cs -> Ghc.EpaComments $ com : cs
          Ghc.EpaCommentsBalanced cs1 cs2 -> Ghc.EpaCommentsBalanced (com : cs1) cs2
     in Ghc.setComments newComms mempty ann

-- | Finds the target pattern and splits it. Returns the modified source and True if successful.
-- Applied post delta transformation.
splitPattern
  :: M.Map Ghc.ModuleName Ghc.ModuleName
  -> [NonExhaustivePattern]
  -> Ghc.ParsedSource
  -> (Ghc.ParsedSource, Any)
splitPattern qualiImps nePats ps =
    Writer.runWriter $
      Syb.everywhereM
        ( Syb.mkM (Writer.writer . goExpr)
          `Syb.extM` (Writer.writer . goDecl)
        ) ps
  where
  isIdxComment (Ghc.L _ (Ghc.EpaComment (Ghc.EpaLineComment str) realSpan))
    = realSpan == Ghc.placeholderRealSpan && all Char.isDigit str
  isIdxComment _ = False

  extractIdxComment (Ghc.EpaComments comms)
    | (before, Ghc.L _ (Ghc.EpaComment (Ghc.EpaLineComment str) _) : rest)
        <- break isIdxComment comms
    , Just idx <- readMaybe str
    , let newComments = Ghc.EpaComments $ before ++ rest
    = Just (idx, newComments)
  extractIdxComment Ghc.EpaCommentsBalanced{} = Nothing
  extractIdxComment _ = Nothing

  goExpr :: Ghc.LHsExpr Ghc.GhcPs -> (Ghc.LHsExpr Ghc.GhcPs, Any)
  goExpr (Ghc.L ann (Ghc.HsCase x scrut matchGroup))
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments ann)
    , Just nePat <- nePats List.!? neIdx
    , Just newMG <- splitMG False False 0 nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.HsCase x scrut newMG)
      , Any True
      )
#if MIN_VERSION_ghc(9,10,0)
  goExpr (Ghc.L ann (Ghc.HsLam x lamType matchGroup@(Ghc.MG _ (Ghc.L matchesAnn _))))
    | lamType /= Ghc.LamSingle
    , Just (neIdx, otherComms) <- extractIdxComment (Ghc.comments ann)
    , Just nePat <- nePats List.!? neIdx
    , Just newMG <- splitMG True False (Ghc.colDelta matchesAnn) nePat matchGroup
    = ( Ghc.L ann {Ghc.comments = otherComms} (Ghc.HsLam x lamType newMG)
      , Any True
      )
#elif MIN_VERSION_ghc(9,8,0)
  goExpr (Ghc.L ann (Ghc.HsLamCase x lamType matchGroup@(Ghc.MG _ (Ghc.L matchesAnn _))))
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments ann)
    , Just nePat <- nePats List.!? neIdx
    , Just newMG <- splitMG True False (Ghc.colDelta matchesAnn) nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.HsLamCase x lamType newMG)
      , Any True
      )
#endif
  goExpr expr = (expr, Any False)

  goDecl :: Ghc.LHsDecl Ghc.GhcPs -> (Ghc.LHsDecl Ghc.GhcPs, Any)
  goDecl (Ghc.L ann (Ghc.ValD x (Ghc.FunBind x2 fid matchGroup)))
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments ann)
    , Just nePat <- nePats List.!? neIdx
    , Just newMG <- splitMG True True 0 nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.ValD x (Ghc.FunBind x2 fid newMG))
      , Any True
      )
  goDecl decl = (decl, Any False)

  splitMG multiplePats offsetFirstPat colOffset nePat (Ghc.MG x2 (Ghc.L ann2 matches))
    | (beforeSplit, Ghc.L splitAnn (Ghc.Match _ ctx _ rhs) : afterSplit)
        <- break matchHasSplit matches
    , let -- If splitting the first match, trailing matches need to have a delta
          -- putting it on a new line
          correctDeltas [] = []
          correctDeltas (x : xs) =
            x :
              (Ghc.L (Ghc.nextLine colOffset) . Ghc.unLoc
               <$> xs)
          newMatches = correctDeltas $ do
            nabla <- patternNablas nePat
            let pats =
                  zipWith (mkPat qualiImps nabla $ not multiplePats)
                          (offsetFirstPat : repeat True)
                          (patternIds nePat)
            [ Ghc.L splitAnn $ Ghc.Match Ghc.noAnn ctx pats rhs ]
          newMatchGroup = beforeSplit ++ newMatches ++ filter (not . matchHasSplit) afterSplit
    = Just $ Ghc.MG x2 (Ghc.L ann2 newMatchGroup)
    | otherwise = Nothing

matchHasSplit :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Bool
matchHasSplit (Ghc.L _ (Ghc.Match _ _ pats _)) = Syb.everything (||) (Syb.mkQ False isSplitCon) pats
  where
  isSplitCon :: Ghc.Pat Ghc.GhcPs -> Bool
  isSplitCon (Ghc.ConPat _ (Ghc.L _ rdr) _) =
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
mkPat qualiImps nabla isOutermost needsLeftPad x = Ghc.L delta $
  case Ghc.lookupSolution nabla x of
    Nothing -> Ghc.WildPat Ghc.noExtField
    Just (Ghc.PACA (Ghc.PmAltConLike con) _tvs args) -> paren args $
      case Ghc.conLikeIsInfix con of
        True | [arg1, arg2] <- args ->
          Ghc.ConPat Ghc.noAnn
            -- []
            ( Ghc.L Ghc.nameAnchorD1
            . nameToRdrName qualiImps
            $ Ghc.conLikeName con
            )
          $ Ghc.InfixCon (mkPat qualiImps nabla False False arg1)
                         (mkPat qualiImps nabla False True arg2)
        _ | Ghc.RealDataCon dc <- con
          , Ghc.isUnboxedTupleDataCon dc
          -> Ghc.TuplePat Ghc.noAnn
--                [ Ghc.AddEpAnn Ghc.AnnOpenPH EP.d0
--                , Ghc.AddEpAnn Ghc.AnnClosePH EP.d0
--                ]
               (addCommaAnns $ zipWith (mkPat qualiImps nabla True) (False : repeat True) args)
               Ghc.Unboxed
        _ | Ghc.RealDataCon dc <- con
          , Ghc.isTupleDataCon dc
          -> Ghc.TuplePat Ghc.noAnn
--                [ Ghc.AddEpAnn Ghc.AnnOpenP EP.d0
--                , Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
--                ]
               (addCommaAnns $ zipWith (mkPat qualiImps nabla True) (False : repeat True) args)
               Ghc.Boxed
        _ ->
          -- If GHC tries to use SPLIT as a missing pattern, replace it with wildcard
          if Ghc.occName (Ghc.conLikeName con) == Ghc.mkDataOcc splitName
          then Ghc.WildPat Ghc.noExtField
          else Ghc.ConPat Ghc.noAnn
                ( Ghc.L Ghc.nameAnchorD0
                . nameToRdrName qualiImps $ Ghc.conLikeName con
                )
             $ Ghc.PrefixCon [] (mkPat qualiImps nabla False True <$> args)
    Just (Ghc.PACA (Ghc.PmAltLit lit) _tvs _args) ->
      case Ghc.pm_lit_val lit of
        Ghc.PmLitInt integer ->
          Ghc.NPat Ghc.noAnn (Ghc.noLocA $ Ghc.OverLit Ghc.noExtField $ Ghc.HsIntegral $ Ghc.IL (Ghc.SourceText . fromString $ show integer) (integer < 0) integer) Nothing Ghc.noExtField
        Ghc.PmLitRat rational ->
          Ghc.NPat Ghc.noAnn (Ghc.noLocA $ Ghc.OverLit Ghc.noExtField $ Ghc.HsFractional $ Ghc.mkTHFractionalLit rational) Nothing Ghc.noExtField
        Ghc.PmLitChar char -> Ghc.LitPat Ghc.noExtField $ Ghc.HsChar Ghc.NoSourceText char
        Ghc.PmLitString fastString -> Ghc.LitPat Ghc.noExtField $ Ghc.HsString Ghc.NoSourceText fastString
        Ghc.PmLitOverInt _minuses integer ->
          Ghc.NPat Ghc.noAnn (Ghc.noLocA $ Ghc.OverLit Ghc.noExtField $ Ghc.HsIntegral $ Ghc.IL (Ghc.SourceText . fromString $ show integer) (integer < 0) integer) Nothing Ghc.noExtField
        Ghc.PmLitOverRat _minuses fractionalLit ->
          Ghc.NPat Ghc.noAnn (Ghc.noLocA $ Ghc.OverLit Ghc.noExtField $ Ghc.HsFractional fractionalLit) Nothing Ghc.noExtField
        Ghc.PmLitOverString fastString -> Ghc.LitPat Ghc.noExtField $ Ghc.HsString Ghc.NoSourceText fastString
  where
    delta = if needsLeftPad
               then Ghc.anchorD1
               else Ghc.anchorD0
    paren [] inner = inner
    paren _ inner =
      if not isOutermost
      then Ghc.mkParPat' (Ghc.L Ghc.anchorD0 inner)
      else inner
    addCommaAnns [] = []
    addCommaAnns [a] = [a]
    addCommaAnns (Ghc.L epAnn a : rest) = Ghc.L (EP.addComma epAnn) a : addCommaAnns rest

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
