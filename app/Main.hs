{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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

test :: Rec -> Bool
test x = case x of
  Rec False _ -> False
  Rec True _ -> False



-- bar :: F.Foo -> F.Foo -> Bool
-- bar F.Foo F.Foo = True
-- bar F.Bar F.Foo = False
