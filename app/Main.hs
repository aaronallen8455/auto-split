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

test :: Bool -> F.Foo -> Bool
test True F.Foo = True
test True F.Bar = True
test True F.Baz = True
test False F.Foo = True
test False F.Bar = True
test False F.Baz = True

-- bar :: F.Foo -> F.Foo -> Bool
-- bar F.Foo F.Foo = True
-- bar F.Bar F.Foo = False
