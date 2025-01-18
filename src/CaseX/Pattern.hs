{-# LANGUAGE PatternSynonyms #-}
module CaseX.Pattern
  ( pattern SPLIT
  ) where

-- | Used to induce the incomplete patterns warning from GHC
pattern SPLIT :: a
pattern SPLIT <- _
