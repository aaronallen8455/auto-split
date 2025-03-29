module Import
  ( Foo(..)
  , R(..)
  ) where

data Foo = Foo | Bar | Baz

data R = R { r1 :: Bool, r2 :: Int }
