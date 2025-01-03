{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Ksplit
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
import           Data.String (fromString)
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified Ksplit.GhcFacade as Ghc

import           Debug.Trace

pattern SPLIT :: a
pattern SPLIT <- _

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = \_ hscEnv -> pure $ addDsHook hscEnv
  , Ghc.parsedResultAction = \_ _ result -> pure $ addImport result
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.renamedResultAction = \_ env groups ->
      removeUnusedImportWarn >> pure (env, groups)
  , Ghc.typeCheckResultAction = \_ _ env ->
      removeUnusedImportWarn >> pure env
  }

addDsHook :: Ghc.HscEnv -> Ghc.HscEnv
addDsHook hscEnv = hscEnv
  { Ghc.hsc_hooks =
      let hooks = Ghc.hsc_hooks hscEnv
       in hooks
          { Ghc.runPhaseHook = Just $ phaseHook (Ghc.runPhaseHook hooks)
          }
  }
  where
    phaseHook mExistingHook = Ghc.PhaseHook $ \tPhase -> do
      --errsVar <- Ghc.getErrsVar
      let isMissingPatWarn msgEnv =
            case Ghc.errMsgDiagnostic msgEnv of
              Ghc.GhcDsMessage (Ghc.DsNonExhaustivePatterns ctx _ _ patIds nablas)
                | Just srcSpan <- Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan msgEnv) -> Left
                NonExhaustivePattern
                  { matchContext = ctx
                  , patternIds = patIds
                  , patternNablas = nablas
                  , srcCodeLoc = srcSpan
                  , origMessage = msgEnv
                  }
              _ -> Right msgEnv
          runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h) mExistingHook
      case tPhase of
        Ghc.T_HscPostTc env modSum tcResult@(Ghc.FrontendTypecheck gblEnv) warns mOldHash -> do
          usedGres <- readIORef $ Ghc.tcg_used_gres gblEnv
          let usesSplit = any ((== "SPLIT") . Ghc.getOccFS . Ghc.gre_name) usedGres
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
                    traverse_ ( \w -> do
                               case splitPattern qualiImps w parsedMod of
                                 (ps, Any True) ->
                                   traverse_ (flip writeFile $ EP.exactPrint ps) mFilePath
                                 _ -> pure ()
                             )
                      missingPatWarns
                  _ -> pure ()
                throw . Ghc.SourceError $ Ghc.mkMessages otherWarns
              )
        _ -> runPhaseOrExistingHook tPhase

-- | Diagnostic thrown when case splitting should be attempted.
data PatternSplitDiag = PatternSplitDiag

instance Ghc.Diagnostic PatternSplitDiag where
  type DiagnosticOpts PatternSplitDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated (Ghc.text "Splitting a pattern (not an error)")
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing

data NonExhaustivePattern = NonExhaustivePattern
  { matchContext :: !Ghc.HsMatchContextRn
  , patternIds :: [Ghc.Id]
  , patternNablas :: [Ghc.Nabla]
  , srcCodeLoc :: Ghc.RealSrcSpan
  , origMessage :: Ghc.MsgEnvelope Ghc.GhcMessage
  }

-- | Finds the target pattern and splits it. Returns the modified source and True if successful.
splitPattern
  :: M.Map Ghc.ModuleName Ghc.ModuleName
  -> NonExhaustivePattern
  -> Ghc.ParsedSource
  -> (Ghc.ParsedSource, Any)
splitPattern qualiImps nePat ps = Writer.runWriter $ Syb.everywhereM (Syb.mkM (Writer.writer . go)) ps
  where
  -- TODO could find the decl that encompasses the warn's span rather than
  -- traversing the entire module AST.
  go :: Ghc.LHsExpr Ghc.GhcPs -> (Ghc.LHsExpr Ghc.GhcPs, Any)
  go expr@(Ghc.L ann (Ghc.HsCase x1 scrut (Ghc.MG x2 (Ghc.L ann2 _))))
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , caseLoc == srcCodeLoc nePat
    , Ghc.L _ (Ghc.HsCase _ _ (Ghc.MG _ (Ghc.L _ matches)))
        <- EP.makeDeltaAst expr
    , (beforeSplit, Ghc.L splitAnn (Ghc.Match _ ctx _ rhs) : afterSplit)
        <- break matchHasSplit matches
    , let -- If splitting the first match, trailing matches need to have a delta
          -- putting it on a new line
          correctDeltas [] = []
          correctDeltas (x : xs) =
            x :
              (Ghc.L (Ghc.EpAnn (Ghc.EpaDelta (Ghc.DifferentLine 1 0) []) Ghc.noAnn Ghc.emptyComments) . Ghc.unLoc
               <$> xs)
          newMatches = correctDeltas $ do
            nabla <- patternNablas nePat
            x <- patternIds nePat
            let pat = mkPat qualiImps nabla True False x
            [ Ghc.L splitAnn $ Ghc.Match [] ctx [pat] rhs ]
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
    = ( Ghc.L ann (Ghc.HsCase x1 scrut (Ghc.MG x2 (Ghc.L (removeDupeComments ann2) newMatchGroup)))
      , Any True
      )
    | otherwise = (expr, Any False)
  go expr = (expr, Any False)

matchHasSplit :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Bool
matchHasSplit = Syb.everything (||) $ Syb.mkQ False isSplitCon
  where
  isSplitCon :: Ghc.Pat Ghc.GhcPs -> Bool
  isSplitCon (Ghc.ConPat [] (Ghc.L _ rdr) _) = rdr == Ghc.mkUnqual Ghc.dataName "SPLIT"
  isSplitCon _ = False

-- TODO write a generic traversal over two things that zips them together in a
-- top down manner

-- | Produce a 'Pat' for a missing pattern
mkPat
  :: M.Map Ghc.ModuleName Ghc.ModuleName
  -> Ghc.Nabla
  -> Bool
  -> Bool
  -> Ghc.Id
  -> Ghc.LPat Ghc.GhcPs
mkPat qualiImps nabla isOutermost needsLeftPad x = Ghc.L (Ghc.EpAnn delta Ghc.noAnn Ghc.emptyComments) $
  case Ghc.lookupSolution nabla x of
    Nothing -> Ghc.WildPat Ghc.noExtField
    Just (Ghc.PACA (Ghc.PmAltConLike con) tvs args) -> paren args $
      -- TODO use literal patterns for lists, tuples, etc
      case Ghc.conLikeIsInfix con of
        True | [arg1, arg2] <- args -> Ghc.ConPat [] (Ghc.L (Ghc.EpAnn EP.d1 Ghc.noAnn Ghc.emptyComments) . nameToRdrName qualiImps $ Ghc.conLikeName con) $
          Ghc.InfixCon (mkPat qualiImps nabla False False arg1) (mkPat qualiImps nabla False True arg2)
        _ -> Ghc.ConPat [] (Ghc.L (Ghc.EpAnn EP.d0 Ghc.noAnn Ghc.emptyComments) . nameToRdrName qualiImps $ Ghc.conLikeName con) $
          Ghc.PrefixCon [] (mkPat qualiImps nabla False True <$> args)
    Just (Ghc.PACA (Ghc.PmAltLit lit) _tvs args) -> Ghc.LitPat Ghc.noExtField $
      case Ghc.pm_lit_val lit of
        Ghc.PmLitInt integer -> Ghc.HsInt Ghc.noExtField (Ghc.IL Ghc.NoSourceText (integer < 0) integer)
        Ghc.PmLitRat rational -> undefined -- TODO
        Ghc.PmLitChar char -> Ghc.HsChar Ghc.NoSourceText char
        Ghc.PmLitString fastString -> Ghc.HsString Ghc.NoSourceText fastString
        Ghc.PmLitOverInt int integer -> undefined -- TODO
        Ghc.PmLitOverRat int fractionalLit -> undefined -- TODO
        Ghc.PmLitOverString fastString -> Ghc.HsString Ghc.NoSourceText fastString
  where
    delta = if needsLeftPad then EP.d1 else EP.d0
    paren [] inner = inner
    paren _ inner =
      if not isOutermost
      then Ghc.ParPat (Ghc.EpTok EP.d0, Ghc.EpTok EP.d0) (Ghc.L (Ghc.EpAnn EP.d0 Ghc.noAnn Ghc.emptyComments) inner)
      else inner

addImport :: Ghc.ParsedResult -> Ghc.ParsedResult
addImport result = result
  { Ghc.parsedResultModule =
    let resMod = Ghc.parsedResultModule result
     in resMod
        { Ghc.hpm_module = Ghc.hpm_module resMod <&> \pMod ->
            pMod
              { Ghc.hsmodImports =
                  ksplitImport : Ghc.hsmodImports pMod
              }
        }
  }
  where
    ksplitImport = Ghc.noLocA . Ghc.simpleImportDecl $ Ghc.mkModuleName "Ksplit"

-- Should insert SPLIT into tcg_used_gres so there is no warning
removeUnusedImportWarn :: Ghc.TcM ()
removeUnusedImportWarn = do
  errsVar <- Ghc.getErrsVar
  let isKsplitImportWarn msgEnv =
        case Ghc.errMsgDiagnostic msgEnv of
          Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnUnusedImport decl _)) ->
            Ghc.unLoc (Ghc.ideclName decl) == Ghc.mkModuleName "Ksplit"
          _ -> False
  Ghc.liftIO . modifyIORef errsVar $
    Ghc.mkMessages . Ghc.filterBag (not . isKsplitImportWarn) . Ghc.getMessages
  pure ()

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
