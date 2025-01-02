module Main where

import qualified Foo as F

main :: IO ()
main = pure ()

foo :: Either (Maybe F.Foo) Int -> Bool
foo x = case x of
  -- test 2
  Left Nothing -> False
  Left (Just F.Foo) -> False
  Left (Just F.Bar) -> False
  Left (Just F.Baz) -> False
  -- test
  Right _ -> True
  -- test 3

bar :: F.Foo -> F.Foo -> Bool
bar F.Foo F.Foo = True
bar F.Bar F.Foo = False
