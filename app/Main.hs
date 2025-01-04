{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Maybe
import qualified Foo as F

main :: IO ()
main = pure ()

foo :: Either (Maybe F.Foo) Int -> Bool
foo x = case x of
  -- test 2
  Left Nothing -> False
  Left (Just _) ->
    False
  -- test
  Right _ -> True
  -- test 3

-- pattern I :: a -> a
-- pattern I a = a
-- 
-- {-# COMPLETE I #-}

pattern J :: a -> Maybe a
pattern J a = Just a

pattern N :: Maybe a
pattern N = Nothing

{-# COMPLETE J, N #-}

data Rec = Rec { one :: Bool, two :: Bool }

test :: Maybe Bool -> Bool
test = _

-- This produces unexpected results:
-- test :: Maybe Bool -> Either F.Foo Bool -> Bool
-- test Nothing (Left F.Foo) = True
-- test Nothing (Left F.Bar) = True
-- test Nothing (Left F.Baz) = True
-- test Nothing (Right False) = True
-- test Nothing (Right True) = True
-- test (Just False) (Left SPLIT) = True
-- test (Just False) (Right SPLIT) = True
-- test (Just True) (Left SPLIT) = True
-- test (Just True) (Right SPLIT) = True

-- bar :: F.Foo -> F.Foo -> Bool
-- bar F.Foo F.Foo = True
-- bar F.Bar F.Foo = False
