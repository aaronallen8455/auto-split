{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AutoSplit
  ( plugin
  ) where

import           Control.Exception
import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.Char as Char
import           Data.Foldable
import           Data.Functor
import qualified Data.Generics as Syb
import           Data.IORef
import qualified Data.List as List
#if MIN_VERSION_ghc(9,12,0)
import qualified Data.List.NonEmpty as NE
#endif
import           Data.Maybe
import           Data.Monoid (Any(..))
import           Data.String (IsString, fromString)
import qualified Data.Typeable as Typeable
import qualified GHC.Paths as Paths
import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Parsers as EP
import qualified Language.Haskell.GHC.ExactPrint.Utils as EP
import           Text.Read (readMaybe)

import qualified AutoSplit.GhcFacade as Ghc

plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.driverPlugin = \_ hscEnv -> pure $ addDsHook hscEnv
  , Ghc.parsedResultAction = \_ _ result -> pure $ addImport result
  , Ghc.pluginRecompile = Ghc.purePlugin
  , Ghc.typeCheckResultAction = \_ _ env ->
      removeUnusedImportWarn >> pure env
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
                | Just srcSpan <- Ghc.srcSpanToRealSrcSpan (Ghc.errMsgSpan msgEnv) ->
                    Left NonExhaustivePatterns
                      { patternIds = patIds
                      , patternNablas = nablas
                      , srcCodeLoc = srcSpan
                      }
              _ -> Right msgEnv
          isAutoSplitError msgEnv =
            case Ghc.errMsgDiagnostic msgEnv of
              Ghc.GhcUnknownMessage (Ghc.UnknownDiagnostic' a)
                | Just PatternSplitDiag <- Typeable.cast a -> True
              _ -> False
          runPhaseOrExistingHook :: Ghc.TPhase res -> IO res
          runPhaseOrExistingHook = maybe Ghc.runPhase (\(Ghc.PhaseHook h) -> h) mExistingHook
      case tPhase of
        Ghc.T_HscPostTc env modSum tcResult@(Ghc.FrontendTypecheck gblEnv) warns mOldHash -> do
          usedGres <- readIORef $ Ghc.tcg_used_gres gblEnv
          let usesSplit = any ((== splitName) . Ghc.occNameFS . Ghc.occName . Ghc.gre_name) usedGres
              mFilePath = Ghc.ml_hs_file (Ghc.ms_location modSum)
          case mFilePath of
            Just filePath | usesSplit -> do
              let patSplitErr =
                    Ghc.mkPlainErrorMsgEnvelope
                      (Ghc.mkGeneralSrcSpan $ fromString filePath)
                      (Ghc.ghcUnknownMessage PatternSplitDiag)
                  noMissingPatErr =
                    Ghc.mkPlainErrorMsgEnvelope
                      (Ghc.mkGeneralSrcSpan $ fromString filePath)
                      (Ghc.ghcUnknownMessage NoMissingPat)
                  parseFailedErr =
                    Ghc.mkPlainErrorMsgEnvelope
                      (Ghc.mkGeneralSrcSpan $ fromString filePath)
                      (Ghc.ghcUnknownMessage ParseFailed)
                  warnsWithError = Ghc.addMessage patSplitErr warns
                  updEnv = env
                    -- Plugin only functions if incomplete patterns warning is enabled, so we force it on.
                    -- If this is instead done as driver plugin, ghci sessions won't pick it up.
                    { Ghc.hsc_dflags = (Ghc.hsc_dflags env `Ghc.wopt_set` Ghc.Opt_WarnIncompletePatterns)
                      -- Need to override the number of uncovered patterns reported.
                      { Ghc.maxUncoveredPatterns = maxBound - 1 }
                    }
              catch
                (runPhaseOrExistingHook $ Ghc.T_HscPostTc updEnv modSum tcResult warnsWithError mOldHash)
                (\(Ghc.SourceError msgs) -> do
                  let (missingPatWarns, otherDiags) = Ghc.partitionBagWith isMissingPatWarn (Ghc.getMessages msgs)
                      -- Need to parse the module because GHC removes
                      -- ParsedResult from ModSummary after the frontend finishes.
                      -- Use the options from compilation to parse the module, otherwise certain
                      -- syntax extensions won't parse correctly.
                      dynFlags = Ghc.ms_hspp_opts modSum `Ghc.gopt_set` Ghc.Opt_KeepRawTokenStream
                  eResult <- parseModule env dynFlags filePath
                  case eResult of
                    (Right parsedMod, usesCpp)
                      | not (null missingPatWarns) -> do
                          let gblRdrEnv = Ghc.tcg_rdr_env gblEnv
                          modifyModule gblRdrEnv parsedMod usesCpp missingPatWarns filePath
                          throw . Ghc.SourceError $ Ghc.mkMessages otherDiags
                      | otherwise -> throw . Ghc.SourceError . Ghc.mkMessages
                                   . Ghc.consBag noMissingPatErr
                                   $ Ghc.filterBag (not . isAutoSplitError) otherDiags
                    (Left _, _) ->
                      throw . Ghc.SourceError . Ghc.mkMessages
                            . Ghc.consBag parseFailedErr
                            $ Ghc.filterBag (not . isAutoSplitError) otherDiags
                )
            _ -> runPhaseOrExistingHook tPhase -- no SPLIT or no file path
        _ -> runPhaseOrExistingHook tPhase

-- | Parse the given module file. Accounts for CPP comments
parseModule
  :: Ghc.HscEnv
  -> Ghc.DynFlags
  -> String
  -> IO (EP.ParseResult Ghc.ParsedSource, Bool)
parseModule env dynFlags filePath = EP.ghcWrapper Paths.libdir $ do
  Ghc.setSession env { Ghc.hsc_dflags = dynFlags }
  res <- EP.parseModuleEpAnnsWithCppInternal EP.defaultCppOptions dynFlags filePath
  let eCppComments = fmap (\(c, _, _) -> c) res
      hasCpp = case eCppComments of
                 Right cs -> not $ null cs
                 _ -> False
  pure
    ( liftA2 EP.insertCppComments
        (EP.postParseTransform res)
        eCppComments
    , hasCpp
    )

-- | Applies pattern split transformation and updates the module file.
modifyModule
  :: Ghc.GlobalRdrEnv
  -> Ghc.ParsedSource
  -> Bool
  -> Ghc.Bag NonExhaustivePatterns
  -> FilePath
  -> IO ()
modifyModule gblRdrEnv parsedMod usesCpp missingPatWarns filePath = do
  let ast = EP.makeDeltaAst
          $ searchAndMark (Ghc.bagToList missingPatWarns) parsedMod
  case splitPattern gblRdrEnv (Ghc.bagToList missingPatWarns) ast of
    (ps, Any True) ->
      -- If the source contains CPP, newlines are appended
      -- to the end of the file when exact printing. The simple
      -- solution is to remove trailing newlines after exact printing
      -- if the source contains CPP comments.
      let removeTrailingNewlines
            | usesCpp =
                reverse . ('\n' :) . dropWhile (== '\n') . reverse
            | otherwise = id
          printed = removeTrailingNewlines $ EP.exactPrint ps
       in writeFile filePath printed
    _ -> pure ()

-- | Diagnostic thrown when case splitting should be attempted.
data PatternSplitDiag = PatternSplitDiag

instance Ghc.Diagnostic PatternSplitDiag where
  type DiagnosticOpts PatternSplitDiag = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module updated by auto-split, compilation aborted"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Diagnostic thrown when SPLIT is used but there are no resulting warnings
data NoMissingPat = NoMissingPat

instance Ghc.Diagnostic NoMissingPat where
  type DiagnosticOpts NoMissingPat = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "Module was not updated because all cases are already covered where SPLIT occurs"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

-- | Diagnostic thrown if the module fails to parse
data ParseFailed = ParseFailed

instance Ghc.Diagnostic ParseFailed where
  type DiagnosticOpts ParseFailed = Ghc.NoDiagnosticOpts
  diagnosticMessage _ _ = Ghc.mkSimpleDecorated $
    Ghc.text "auto-split failed to parse the module"
  diagnosticReason _ = Ghc.ErrorWithoutFlag
  diagnosticHints _ = []
  diagnosticCode _ = Nothing
#if !MIN_VERSION_ghc(9,8,0)
  defaultDiagnosticOpts = Ghc.NoDiagnosticOpts
#endif

data NonExhaustivePatterns = NonExhaustivePatterns
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
  :: [NonExhaustivePatterns]
  -> Ghc.ParsedSource
  -> Ghc.ParsedSource
searchAndMark nePats =
    Syb.everywhere (Syb.mkT goExpr `Syb.extT` goDecl `Syb.extT` goBind)
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
#elif MIN_VERSION_ghc(9,6,0)
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

  goBind :: Ghc.LHsBind Ghc.GhcPs -> Ghc.LHsBind Ghc.GhcPs
  goBind (Ghc.L ann f@Ghc.FunBind{})
    | Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just neIdx <- List.findIndex ((caseLoc ==) . srcCodeLoc) nePats
    = Ghc.L (addIndexComment ann neIdx) f
  goBind x = x

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
  :: Ghc.GlobalRdrEnv
  -> [NonExhaustivePatterns]
  -> Ghc.ParsedSource
  -> (Ghc.ParsedSource, Any)
splitPattern gblRdrEnv nePats ps =
    Writer.runWriter $
      Syb.everywhereM
        ( Syb.mkM (Writer.writer . goExpr)
          `Syb.extM` (Writer.writer . goDecl)
          `Syb.extM` (Writer.writer . goBind)
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
    , Just nePat <- listToMaybe $ drop neIdx nePats
    , Just newMG <- splitMG gblRdrEnv False False 0 nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.HsCase x scrut newMG)
      , Any True
      )
#if MIN_VERSION_ghc(9,10,0)
  goExpr (Ghc.L ann (Ghc.HsLam x lamType matchGroup@(Ghc.MG _ (Ghc.L matchesAnn _))))
    | lamType /= Ghc.LamSingle
    , Just (neIdx, otherComms) <- extractIdxComment (Ghc.comments ann)
    , Just nePat <- nePats List.!? neIdx
    , Just newMG <- splitMG gblRdrEnv (lamType == Ghc.LamCases) False (Ghc.colDelta matchesAnn) nePat matchGroup
    = ( Ghc.L ann {Ghc.comments = otherComms} (Ghc.HsLam x lamType newMG)
      , Any True
      )
#elif MIN_VERSION_ghc(9,6,0)
  goExpr (Ghc.L ann (Ghc.HsLamCase x lamType matchGroup@(Ghc.MG _ (Ghc.L matchesAnn _))))
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments ann)
    , Just nePat <- listToMaybe $ drop neIdx nePats
    , Just newMG <- splitMG gblRdrEnv (lamType == Ghc.LamCases) False (Ghc.colDelta matchesAnn) nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.HsLamCase x lamType newMG)
      , Any True
      )
#endif
  goExpr expr = (expr, Any False)

  goDecl :: Ghc.LHsDecl Ghc.GhcPs -> (Ghc.LHsDecl Ghc.GhcPs, Any)
  goDecl (Ghc.L ann (Ghc.ValD x (Ghc.FunBind x2 fid matchGroup)))
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments ann)
    , Just nePat <- listToMaybe $ drop neIdx nePats
    , Just newMG <- splitMG gblRdrEnv True True 0 nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.ValD x (Ghc.FunBind x2 fid newMG))
      , Any True
      )
  goDecl decl = (decl, Any False)

  goBind :: Ghc.LHsBind Ghc.GhcPs -> (Ghc.LHsBind Ghc.GhcPs, Any)
  goBind (Ghc.L ann (Ghc.FunBind x2 fid matchGroup))
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments ann)
    , Just nePat <- listToMaybe $ drop neIdx nePats
    , Just newMG <- splitMG gblRdrEnv True True 0 nePat matchGroup
    = ( Ghc.L (Ghc.setComments otherComms mempty ann) (Ghc.FunBind x2 fid newMG)
      , Any True
      )
  goBind bind = (bind, Any False)

splitMG
  :: Ghc.GlobalRdrEnv
  -> Bool -- True => match group can have multiple patterns
  -> Bool -- True => add left padding to each new pattern
  -> Int -- Number of horizontal spaces at the front of inserted pattern match
  -> NonExhaustivePatterns
  -> Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
  -> Maybe (Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs))
splitMG gblRdrEnv multiplePats offsetFirstPat colOffset nePat (Ghc.MG x2 (Ghc.L ann2 matches))
  | (beforeSplit, Ghc.L splitAnn targetMatch@(Ghc.Match _ ctx _ rhs) : afterSplit)
      <- break matchHasSplit matches
  , let mUpdatedMatch = Ghc.L splitAnn <$> removeSplitFromOrPat targetMatch
        -- If splitting the first match, trailing matches need to have a delta
        -- putting it on a new line
        correctDeltas [] = []
        correctDeltas (x : xs) | isNothing mUpdatedMatch =
          x :
            (Ghc.L (Ghc.nextLine colOffset) . Ghc.unLoc
             <$> xs)
        correctDeltas xs = Ghc.L (Ghc.nextLine colOffset) . Ghc.unLoc <$> xs
        newMatches = correctDeltas $ do
          nabla <- patternNablas nePat
          let pats =
                zipWith (mkPat gblRdrEnv nabla $ not multiplePats)
                        (offsetFirstPat : repeat True)
                        (patternIds nePat)
          [ Ghc.L splitAnn $ Ghc.Match Ghc.noExtFieldCpp ctx (Ghc.noLocCpp pats) rhs ]
        removeSplits m =
          case traverse removeSplitFromOrPat m of
            Nothing -> if matchHasSplit m then Nothing else Just m
            Just newM -> Just newM
        afterSplitUpdated = mapMaybe removeSplits afterSplit
        newMatchGroup
          = beforeSplit
         ++ maybeToList mUpdatedMatch
         ++ newMatches
         ++ afterSplitUpdated
  = Just $ Ghc.MG x2 (Ghc.L ann2 newMatchGroup)
  | otherwise = Nothing

matchHasSplit :: Ghc.LMatch Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs) -> Bool
matchHasSplit (Ghc.L _ (Ghc.Match _ _ pats _)) =
    Syb.everything (||) (Syb.mkQ False isSplitCon) pats

isSplitCon :: Ghc.Pat Ghc.GhcPs -> Bool
isSplitCon (Ghc.ConPat _ (Ghc.L _ rdr) _) =
  Ghc.rdrNameOcc rdr == Ghc.mkDataOcc splitName
isSplitCon _ = False

-- | Remove SPLIT pattern from OrPat groups. Returns Just if the match was modified.
removeSplitFromOrPat
  :: Ghc.Match Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
  -> Maybe (Ghc.Match Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs))
#if MIN_VERSION_ghc(9,12,0)
removeSplitFromOrPat (Ghc.Match a b pats c) =
  let mNewPats = Syb.everywhereM (Syb.mkM (Writer.writer . go)) pats
      patHasSplit = Syb.everything (||) (Syb.mkQ False isSplitCon)
      dropSemicolon ann = ann { Ghc.anns = Ghc.noAnn }
      go :: Ghc.Pat Ghc.GhcPs -> (Ghc.Pat Ghc.GhcPs, Any)
      go (Ghc.OrPat x oPats) =
        case reverse (NE.filter (not . patHasSplit) oPats) of
          [] -> (Ghc.WildPat Ghc.noExtField, Any True)
          (Ghc.L patAnn lastPat : fPats)
            | length fPats + 1 /= NE.length oPats ->
                ( Ghc.OrPat x
                    (NE.reverse $ Ghc.L (dropSemicolon patAnn) lastPat NE.:| fPats)
                , Any True)
            | otherwise -> (Ghc.OrPat x oPats, Any False)
      go other = (other, Any False)
   in case Writer.runWriter mNewPats of
        (newPats, Any True) -> Just (Ghc.Match a b newPats c)
        _ -> Nothing
#else
removeSplitFromOrPat _ = Nothing
#endif

-- | Produce a 'Pat' for a missing pattern
mkPat
  :: Ghc.GlobalRdrEnv
  -> Ghc.Nabla
  -> Bool -- ^ True => is a singular pattern which doesn't need outer parens
  -> Bool -- ^ True => needs left padding to separate it from another pattern
  -> Ghc.Id
  -> Ghc.LPat Ghc.GhcPs
mkPat gblRdrEnv nabla isOutermost needsLeftPad x = Ghc.L delta $
  case Ghc.lookupSolution nabla x of
    Nothing -> Ghc.WildPat Ghc.noExtField
    Just (Ghc.PACA (Ghc.PmAltConLike con) _tvs args) -> paren con args $
      case Ghc.conLikeIsInfix con of
        True | [arg1, arg2] <- args ->
          Ghc.ConPat Ghc.noAnn
            ( Ghc.L Ghc.nameAnchorD1
            . nameToRdrName gblRdrEnv
            $ Ghc.conLikeName con
            )
          $ Ghc.InfixCon (mkPat gblRdrEnv nabla False False arg1)
                         (mkPat gblRdrEnv nabla False True arg2)
        _ | Ghc.RealDataCon dc <- con
          , Ghc.isUnboxedTupleDataCon dc
          -> Ghc.TuplePat Ghc.parenHashAnns
               (addCommaAnns $ zipWith (mkPat gblRdrEnv nabla True) (False : repeat True) args)
               Ghc.Unboxed
        _ | Ghc.RealDataCon dc <- con
          , Ghc.isTupleDataCon dc
          -> Ghc.TuplePat Ghc.parenAnns
               (addCommaAnns $ zipWith (mkPat gblRdrEnv nabla True) (False : repeat True) args)
               Ghc.Boxed
        _ ->
          -- If GHC tries to use SPLIT as a missing pattern, replace it with wildcard
          if Ghc.occName (Ghc.conLikeName con) == Ghc.mkDataOcc splitName
          then Ghc.WildPat Ghc.noExtField
          else Ghc.ConPat Ghc.noAnn
                ( Ghc.L Ghc.nameAnchorD0
                . nameToRdrName gblRdrEnv $ Ghc.conLikeName con
                )
             $ Ghc.PrefixCon [] (mkPat gblRdrEnv nabla False True <$> args)
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
    paren _ [] inner = inner
    paren (Ghc.RealDataCon dc) _ inner | Ghc.isTupleDataCon dc = inner -- No parens for tuple pats
    paren _ _ inner =
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
                  patternImport : Ghc.hsmodImports pMod
              }
        }
  }
  where
    patternImport = Ghc.noLocA . Ghc.simpleImportDecl $ Ghc.mkModuleName patternModName

-- | The automatically added import gets flagged as unused even if it is used.
-- The solution here is to simply suppress the warning.
removeUnusedImportWarn :: Ghc.TcM ()
removeUnusedImportWarn = do
  errsVar <- Ghc.getErrsVar
#if MIN_VERSION_ghc(9,8,0)
  let isAutoSplitImportWarn msgEnv =
        case Ghc.errMsgDiagnostic msgEnv of
          Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnUnusedImport decl _)) ->
            Ghc.unLoc (Ghc.ideclName decl) == Ghc.mkModuleName patternModName
          _ -> False
  Ghc.liftIO . modifyIORef errsVar $
    Ghc.mkMessages . Ghc.filterBag (not . isAutoSplitImportWarn) . Ghc.getMessages
#else
  -- 9.6 lacks the specific diagnostic
  let isAutoSplitImportWarn msgEnv =
        case Ghc.errMsgDiagnostic msgEnv of
          Ghc.TcRnMessageWithInfo _ (Ghc.TcRnMessageDetailed _ (Ghc.TcRnUnknownMessage (Ghc.UnknownDiagnostic diag)))
            | Ghc.WarningWithFlag Ghc.Opt_WarnUnusedImports <- Ghc.diagnosticReason diag
            -> True
          _ -> False
  Ghc.liftIO . modifyIORef errsVar $ \msgs ->
    -- the target import warning always shows up as the last occurrence
    case break isAutoSplitImportWarn . reverse . Ghc.bagToList $ Ghc.getMessages msgs of
      (before, _ : after) -> Ghc.mkMessages . Ghc.listToBag . reverse $ before ++ after
      _ -> msgs
#endif

nameToRdrName :: Ghc.GlobalRdrEnv -> Ghc.Name -> Ghc.RdrName
nameToRdrName rdrEnv n =
  case Ghc.lookupOccEnv rdrEnv occName of
    Just gres
      | Just gre <- find greMatches gres
      , rdrName : _ <- Ghc.greRdrNames gre
      -> rdrName
    Nothing
      | not (Ghc.isWiredInName n)
      , not (Ghc.isBuiltInSyntax n)
      , not (Ghc.isSystemName n)
      , not (Ghc.isInternalName n)
      -> Ghc.mkRdrQual (Ghc.mkModuleName "NOT_IN_SCOPE") occName
    _ -> Ghc.nameRdrName n
  where
    occName = Ghc.getOccName n
    greMatches gre = Ghc.greToName gre == n

splitName :: IsString a => a
splitName = "SPLIT"

patternModName :: IsString a => a
patternModName = "AutoSplit.Pattern"
