{-# LANGUAGE PatternSynonyms #-}
module AutoSplit.Pattern
  ( pattern SPLIT
  , pattern FIELDS
  ) where

-- | Used to induce the incomplete patterns warning from GHC so that the plugin
-- can modify the source code to contain all the missing patterns.
pattern SPLIT :: a
pattern SPLIT <- _

-- | When applied to a record initialization with missing fields, the plugin
-- will add the missing fields. It must be applied directly to the record
-- initialization, i.e. no parens or use of @$@.
pattern FIELDS :: a -> a
pattern FIELDS a = a
