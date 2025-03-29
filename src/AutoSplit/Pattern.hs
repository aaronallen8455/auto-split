{-# LANGUAGE PatternSynonyms #-}
module AutoSplit.Pattern
  ( pattern SPLIT
  , pattern FIELDS
  ) where

-- | Used to induce the incomplete patterns warning from GHC
pattern SPLIT :: a
pattern SPLIT <- _

pattern FIELDS :: a -> a
pattern FIELDS a = a
