{-# LANGUAGE CPP #-}
module AutoSplit.Shared
  ( isIdxComment
  , extractIdxComment
  , addIndexComment
  , nameToRdrName
  , addCommaAnns
  ) where

import           Data.Foldable
import qualified Data.Char as Char
import qualified Language.Haskell.GHC.ExactPrint as EP
import           Text.Read (readMaybe)

import qualified AutoSplit.GhcFacade as Ghc

isIdxComment :: Ghc.GenLocated l Ghc.EpaComment -> Bool
isIdxComment (Ghc.L _ (Ghc.EpaComment (Ghc.EpaLineComment str) realSpan))
  = realSpan == Ghc.placeholderRealSpan && all Char.isDigit str
isIdxComment _ = False

extractIdxComment :: Ghc.EpAnnComments -> Maybe (Int, Ghc.EpAnnComments)
extractIdxComment (Ghc.EpaComments comms)
  | (before, Ghc.L _ (Ghc.EpaComment (Ghc.EpaLineComment str) _) : rest)
      <- break isIdxComment comms
  , Just idx <- readMaybe str
  , let newComments = Ghc.EpaComments $ before ++ rest
  = Just (idx, newComments)
extractIdxComment Ghc.EpaCommentsBalanced{} = Nothing
extractIdxComment _ = Nothing

#if MIN_VERSION_ghc(9,10,0)
addIndexComment :: Ghc.EpAnn ann -> Int -> Ghc.EpAnn ann
#else
addIndexComment :: Monoid ann => Ghc.SrcSpanAnn' (Ghc.EpAnn ann) -> Int -> Ghc.SrcSpanAnn' (Ghc.EpAnn ann)
#endif
addIndexComment ann neIdx =
  let com :: Ghc.LEpaComment
      com = Ghc.L Ghc.fakeCommentLocation
        (Ghc.EpaComment (Ghc.EpaLineComment (show neIdx)) Ghc.placeholderRealSpan)
      newComms = case Ghc.getComments ann of
        Ghc.EpaComments cs -> Ghc.EpaComments $ com : cs
        Ghc.EpaCommentsBalanced cs1 cs2 -> Ghc.EpaCommentsBalanced (com : cs1) cs2
   in Ghc.setComments newComms mempty ann

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

addCommaAnns :: [Ghc.GenLocated Ghc.SrcSpanAnnA e] -> [Ghc.GenLocated Ghc.SrcSpanAnnA e]
addCommaAnns [] = []
addCommaAnns [a] = [a]
addCommaAnns (Ghc.L epAnn a : rest) = Ghc.L (EP.addComma epAnn) a : addCommaAnns rest
