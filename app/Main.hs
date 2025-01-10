{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Maybe
import           Data.Void
import qualified Foo as F

main :: IO ()
main = pure ()

test :: Maybe Bool -> Bool
test x = --hi
 case x of
  -- 1
  Nothing -> False
  -- 2
  Just SPLIT -> True
  -- 3

-- bar :: F.Foo -> F.Foo -> Bool
-- bar F.Foo F.Foo = True
-- bar F.Bar F.Foo = False
