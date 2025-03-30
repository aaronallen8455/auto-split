{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AutoSplit.Fields
  ( MissingFields(..)
  , modifyModule
  , fieldsName
  ) where

import           Control.Monad
import qualified Control.Monad.Trans.Writer.CPS as Writer
import qualified Data.Generics as Syb
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid (Any(..))
import           Data.String (IsString)
import qualified Language.Haskell.GHC.ExactPrint as EP

import qualified AutoSplit.GhcFacade as Ghc
import           AutoSplit.Shared

data MissingFields = MissingFields
  { conLike :: Ghc.ConLike
  , srcCodeLoc :: Ghc.RealSrcSpan
  }

-- | Applies field enumeration transformation and updates the module file.
modifyModule
  :: Ghc.GlobalRdrEnv
  -> Ghc.ParsedSource
  -> Bool
  -> Ghc.Bag MissingFields
  -> FilePath
  -> IO Bool -- ^ True if module was updated
modifyModule gblRdrEnv parsedMod usesCpp missingFields filePath = do
  let ast = EP.makeDeltaAst
          $ searchAndMark (Ghc.bagToList missingFields) parsedMod
  case enumFields gblRdrEnv (Ghc.bagToList missingFields) ast of
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

searchAndMark
  :: [MissingFields]
  -> Ghc.ParsedSource
  -> Ghc.ParsedSource
searchAndMark missingFields =
    Syb.everywhere (Syb.mkT goExpr)
  where
  goExpr :: Ghc.LHsExpr Ghc.GhcPs -> Ghc.LHsExpr Ghc.GhcPs
  goExpr (Ghc.L outerAnn
           a@(Ghc.HsApp _
               (Ghc.L _ (Ghc.HsVar _ (Ghc.L _ rdrName)))
               (Ghc.L ann Ghc.RecordCon{}))
         )
    | Ghc.rdrNameOcc rdrName == Ghc.mkDataOcc fieldsName
    , Just caseLoc <- Ghc.srcSpanToRealSrcSpan $ Ghc.locA ann
    , Just neIdx <- List.findIndex ((caseLoc ==) . srcCodeLoc) missingFields
    = Ghc.L (addIndexComment outerAnn neIdx) a
  goExpr x = x

enumFields
  :: Ghc.GlobalRdrEnv
  -> [MissingFields]
  -> Ghc.ParsedSource
  -> (Ghc.ParsedSource, Any)
enumFields gblRdrEnv missingFields ps =
    Writer.runWriter $
      Syb.everywhereM ( Syb.mkM (Writer.writer . goExpr)) ps
  where
  goExpr :: Ghc.LHsExpr Ghc.GhcPs -> (Ghc.LHsExpr Ghc.GhcPs, Any)
  goExpr (Ghc.L outerAnn
           (Ghc.HsApp _
             (Ghc.L _ (Ghc.HsVar _ _))
             (Ghc.L _ann (Ghc.RecordCon x cLike (Ghc.HsRecFields{..}))))
         )
    | Just (neIdx, otherComms) <- extractIdxComment (Ghc.getComments outerAnn)
    , Just mf <- listToMaybe $ drop neIdx missingFields
    , let newFields = addMissingFields gblRdrEnv mf rec_flds
    = ( Ghc.L (Ghc.setComments otherComms mempty outerAnn) (Ghc.RecordCon x cLike
          (Ghc.HsRecFields {Ghc.rec_flds = newFields, ..}))
      , Any True
      )
  goExpr x = (x, Any False)

addMissingFields
  :: Ghc.GlobalRdrEnv
  -> MissingFields
  -> [Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)]
  -> [Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)]
addMissingFields gblRdrEnv missingFields existing =
  let fieldNames = Ghc.flSelector
               <$> Ghc.conLikeFieldLabels (conLike missingFields) :: [Ghc.Name]
      existingOccNames =
        Ghc.occNameFS . Ghc.rdrNameOcc . Ghc.unLoc . Ghc.foLabel
        . Ghc.unLoc . Ghc.hfbLHS . Ghc.unLoc <$> existing
      toFieldBind :: Ghc.Name -> Maybe (Ghc.LHsRecField Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs))
      toFieldBind name = do
        guard $ Ghc.getOccFS name `notElem` existingOccNames
        Just $ Ghc.noLocA
          Ghc.HsFieldBind
          { Ghc.hfbAnn = Ghc.equalsAnns
          , Ghc.hfbLHS = Ghc.L Ghc.anchorD1 $ Ghc.FieldOcc Ghc.noExtField (Ghc.noLocA $ nameToRdrName gblRdrEnv name)
          , Ghc.hfbRHS = Ghc.L Ghc.anchorD1 $ Ghc.HsVar Ghc.noExtField (Ghc.noLocA . Ghc.mkRdrUnqual $ Ghc.mkVarOcc "_")
          , Ghc.hfbPun = False
          }
   in case reverse existing of
        (r : rs) -> reverse rs ++ addCommaAnns (r : mapMaybe toFieldBind fieldNames)
        _ -> addCommaAnns (mapMaybe toFieldBind fieldNames)

fieldsName :: IsString a => a
fieldsName = "FIELDS"
