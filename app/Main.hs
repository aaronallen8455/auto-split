{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Maybe
import           Data.Void
import qualified Foo as F

main :: IO ()
main = pure ()

test2 :: Maybe Bool -> Bool -> Bool
test2 x y = case x of
  Nothing ->
    case y of
      SPLIT -> False
  -- test
  Just True -> True
  Just False -> True

-- bar :: F.Foo -> F.Foo -> Bool
-- bar F.Foo F.Foo = True
-- bar F.Bar F.Foo = False
