{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module AutoSplit.Split
  ( NonExhaustivePatterns(..)
  , modifyModule
  , splitName
  ) where

import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.Generics as Syb
import qualified Data.List as List
#if MIN_VERSION_ghc(9,12,0)
import qualified Data.List.NonEmpty as NE
#endif
import           Data.Maybe
import           Data.Monoid (Any(..))
import           Data.String (IsString, fromString)
import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified AutoSplit.GhcFacade as Ghc
import           AutoSplit.Shared

data NonExhaustivePatterns = NonExhaustivePatterns
  { patternIds :: [Ghc.Id]
  , patternNablas :: [Ghc.Nabla]
  , srcCodeLoc :: Ghc.RealSrcSpan
  }

-- | Applies pattern split transformation and updates the module file.
modifyModule
  :: Ghc.GlobalRdrEnv
  -> Ghc.ParsedSource
  -> Bool
  -> Ghc.Bag NonExhaustivePatterns
  -> FilePath
  -> IO Bool -- ^ True if module was updated
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
       in True <$ writeFile filePath printed
    _ -> pure False

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

splitName :: IsString a => a
splitName = "SPLIT"
