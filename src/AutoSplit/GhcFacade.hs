{-# LANGUAGE CPP #-}
module AutoSplit.GhcFacade
  ( module Ghc
  , mkParPat'
  , anchorD0
  , anchorD1
  , nameAnchorD0
  , nameAnchorD1
  , colDelta
  , nextLine
  , getComments
  , setComments
  , fakeCommentLocation
  , parenAnns
  , parenHashAnns
  , greToName
  , noLocCpp
  , noExtFieldCpp
  ) where

#if MIN_VERSION_ghc(9,8,0)
import           GHC.Driver.DynFlags as Ghc
import           GHC.Types.Var as Ghc
#else
import           GHC.Driver.Flags as Ghc
import           GHC.Types.Var as Ghc hiding (varName)
import           GHC.Driver.Session as Ghc
#endif
import           GHC as Ghc (ParsedSource)
import           GHC.Core.ConLike as Ghc
import           GHC.Core.DataCon as Ghc
import           GHC.Data.Bag as Ghc
import           GHC.Driver.Env as Ghc
import           GHC.Driver.Errors.Types as Ghc
import           GHC.Driver.Hooks as Ghc
import           GHC.Driver.Main as Ghc
import           GHC.Driver.Pipeline.Execute as Ghc
import           GHC.Driver.Pipeline.Phases as Ghc
import           GHC.Driver.Plugins as Ghc
import           GHC.Driver.Monad as Ghc
import           GHC.Hs as Ghc
import           GHC.HsToCore.Errors.Types as Ghc
import           GHC.HsToCore.Pmc.Solver.Types as Ghc
import           GHC.Tc.Errors.Types as Ghc
import           GHC.Tc.Utils.Monad as Ghc hiding (DefaultingPlugin, TcPlugin)
import           GHC.Types.Error as Ghc
import           GHC.Types.Name as Ghc
import           GHC.Types.Name.Reader as Ghc
import           GHC.Types.SourceError as Ghc
import           GHC.Types.SourceText as Ghc
import           GHC.Types.SrcLoc as Ghc
import           GHC.Unit.Module.Location as Ghc
import           GHC.Unit.Module.ModSummary as Ghc
import           GHC.Unit.Types as Ghc
import           GHC.Utils.Error as Ghc
import           GHC.Utils.Outputable as Ghc
import           Language.Haskell.Syntax.Basic as Ghc

#if !MIN_VERSION_ghc(9,10,0)
import           Data.Maybe
#endif
import qualified Language.Haskell.GHC.ExactPrint as EP

mkParPat' :: Ghc.LPat Ghc.GhcPs -> Ghc.Pat Ghc.GhcPs
#if MIN_VERSION_ghc(9,10,0)
mkParPat' inner = Ghc.ParPat (Ghc.EpTok EP.d0, Ghc.EpTok EP.d0) inner
#elif MIN_VERSION_ghc(9,6,0)
mkParPat' inner = Ghc.ParPat Ghc.noAnn (Ghc.L (Ghc.TokenLoc EP.d0) Ghc.HsTok) inner (Ghc.L (Ghc.TokenLoc EP.d0) Ghc.HsTok)
#endif

anchorD0
#if MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoAnn ann => Ghc.EpAnn ann
anchorD0 = Ghc.EpAnn EP.d0 Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  :: Ghc.SrcSpanAnnA
anchorD0 =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 0)))
      mempty
      Ghc.emptyComments
    )
    Ghc.generatedSrcSpan
#endif

anchorD1
#if MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoAnn ann => Ghc.EpAnn ann
anchorD1 = Ghc.EpAnn EP.d1 Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  :: Ghc.SrcSpanAnnA
anchorD1 =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 1)))
      mempty
      Ghc.emptyComments
    )
    Ghc.generatedSrcSpan
#endif

nameAnchorD0
#if MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoAnn ann => Ghc.EpAnn ann
nameAnchorD0 = Ghc.EpAnn EP.d0 Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  :: Ghc.SrcSpanAnnN
nameAnchorD0 =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 0)))
      mempty
      Ghc.emptyComments
    )
    Ghc.generatedSrcSpan --Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 0))
#endif

nameAnchorD1
#if MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoAnn ann => Ghc.EpAnn ann
nameAnchorD1 = Ghc.EpAnn EP.d1 Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  :: Ghc.SrcSpanAnnN
nameAnchorD1 =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 1)))
      mempty
      Ghc.emptyComments
    )
    Ghc.generatedSrcSpan --Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 0))
#endif

nextLine
  :: Int
#if MIN_VERSION_ghc(9,12,0)
  -> Ghc.NoAnn ann => Ghc.EpAnn ann
nextLine colOffset =
  Ghc.EpAnn (Ghc.EpaDelta Ghc.noSrcSpan (Ghc.DifferentLine 1 colOffset) []) Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,10,0)
  -> Ghc.NoAnn ann => Ghc.EpAnn ann
nextLine colOffset =
  Ghc.EpAnn (Ghc.EpaDelta (Ghc.DifferentLine 1 colOffset) []) Ghc.noAnn Ghc.emptyComments
#elif MIN_VERSION_ghc(9,6,0)
  -> Ghc.SrcSpanAnnA
nextLine colOffset =
  Ghc.SrcSpanAnn
    (Ghc.EpAnn
      (Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.DifferentLine 1 colOffset)))
      mempty
      Ghc.emptyComments
    )
    Ghc.generatedSrcSpan --Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.SameLine 0))
#endif

#if MIN_VERSION_ghc(9,12,0)
colDelta :: Ghc.EpAnn ann -> Int
colDelta (Ghc.EpAnn (Ghc.EpaDelta _ delta _) _ _)
#elif MIN_VERSION_ghc(9,10,0)
colDelta :: Ghc.EpAnn ann -> Int
colDelta (Ghc.EpAnn (Ghc.EpaDelta delta _) _ _)
#elif MIN_VERSION_ghc(9,6,0)
colDelta :: Ghc.SrcSpanAnn' (Ghc.EpAnn ann) -> Int
colDelta (Ghc.SrcSpanAnn (Ghc.EpAnn (Ghc.Anchor _ (Ghc.MovedAnchor delta)) _ _) _)
#endif
  = case delta of
    Ghc.DifferentLine _ c -> c
    Ghc.SameLine c -> c
colDelta _ = 0

#if MIN_VERSION_ghc(9,10,0)
getComments :: Ghc.EpAnn ann -> Ghc.EpAnnComments
getComments = Ghc.comments
#elif MIN_VERSION_ghc(9,6,0)
getComments :: Ghc.SrcSpanAnn' (Ghc.EpAnn ann) -> Ghc.EpAnnComments
getComments a = case Ghc.ann a of
                  Ghc.EpAnnNotUsed -> Ghc.emptyComments
                  epAn -> Ghc.comments epAn
#endif

#if MIN_VERSION_ghc(9,10,0)
setComments :: Ghc.EpAnnComments -> () -> Ghc.EpAnn ann -> Ghc.EpAnn ann
setComments comms () a = a {Ghc.comments = comms}
#elif MIN_VERSION_ghc(9,6,0)
setComments :: Ghc.EpAnnComments -> ann -> Ghc.SrcSpanAnn' (Ghc.EpAnn ann) -> Ghc.SrcSpanAnn' (Ghc.EpAnn ann)
setComments comms defAnn a =
  case Ghc.ann a of
    Ghc.EpAnnNotUsed ->
      a { Ghc.ann = Ghc.EpAnn
            (Ghc.Anchor
              (fromMaybe Ghc.placeholderRealSpan . Ghc.srcSpanToRealSrcSpan $ Ghc.locA a)
              Ghc.UnchangedAnchor
            ) defAnn comms
        }
    _ -> a {Ghc.ann = (Ghc.ann a) {Ghc.comments = comms}}
#endif

-- | A location used for the comments that are inserted for targeted elements.
-- By using a negative column delta, the layout is not affected.
fakeCommentLocation
#if MIN_VERSION_ghc(9,12,0)
  :: Ghc.NoCommentsLocation
fakeCommentLocation = Ghc.EpaDelta Ghc.noSrcSpan (Ghc.DifferentLine (-1) 0) Ghc.NoComments
#elif MIN_VERSION_ghc(9,10,0)
  :: Ghc.NoCommentsLocation
fakeCommentLocation = Ghc.EpaDelta (Ghc.DifferentLine (-1) 0) Ghc.NoComments
#elif MIN_VERSION_ghc(9,6,0)
  :: Ghc.Anchor
fakeCommentLocation = Ghc.Anchor Ghc.placeholderRealSpan (Ghc.MovedAnchor (Ghc.DifferentLine (-1) 0))
#endif

#if MIN_VERSION_ghc(9,12,0)
parenAnns :: (Ghc.EpaLocation, Ghc.EpaLocation)
parenAnns = (EP.d0, EP.d0)

parenHashAnns :: (Ghc.EpaLocation, Ghc.EpaLocation)
parenHashAnns = (EP.d0, EP.d0)
#elif MIN_VERSION_ghc(9,10,0)
parenAnns :: [Ghc.AddEpAnn]
parenAnns =
  [ Ghc.AddEpAnn Ghc.AnnOpenP EP.d0
  , Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
  ]

parenHashAnns :: [Ghc.AddEpAnn]
parenHashAnns =
  [ Ghc.AddEpAnn Ghc.AnnOpenPH EP.d0
  , Ghc.AddEpAnn Ghc.AnnClosePH EP.d0
  ]
#else
parenAnns :: Ghc.EpAnn [Ghc.AddEpAnn]
parenAnns = Ghc.EpAnn
  { Ghc.entry = Ghc.Anchor Ghc.placeholderRealSpan EP.m0
  , Ghc.anns =
      [ Ghc.AddEpAnn Ghc.AnnOpenP EP.d0
      , Ghc.AddEpAnn Ghc.AnnCloseP EP.d0
      ]
  , Ghc.comments = Ghc.emptyComments
  }

parenHashAnns :: Ghc.EpAnn [Ghc.AddEpAnn]
parenHashAnns = Ghc.EpAnn
  { Ghc.entry = Ghc.Anchor Ghc.placeholderRealSpan EP.m0
  , Ghc.anns =
      [ Ghc.AddEpAnn Ghc.AnnOpenPH EP.d0
      , Ghc.AddEpAnn Ghc.AnnClosePH EP.d0
      ]
  , Ghc.comments = Ghc.emptyComments
  }
#endif

greToName :: Ghc.GlobalRdrElt -> Ghc.Name
greToName =
#if MIN_VERSION_ghc(9,8,0)
  Ghc.greName
#else
  Ghc.grePrintableName
#endif

#if MIN_VERSION_ghc(9,12,0)
noLocCpp :: Ghc.HasAnnotation e => a -> Ghc.GenLocated e a
noLocCpp = Ghc.noLocA
#else
noLocCpp :: a -> a
noLocCpp = id
#endif

#if MIN_VERSION_ghc(9,12,0)
noExtFieldCpp :: Ghc.NoExtField
noExtFieldCpp = Ghc.noExtField
#elif MIN_VERSION_ghc(9,10,0)
noExtFieldCpp :: [a]
noExtFieldCpp = []
#else
noExtFieldCpp :: Ghc.EpAnn a
noExtFieldCpp = Ghc.noAnn
#endif
