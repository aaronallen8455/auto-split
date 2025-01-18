module Main where

import qualified Bar

main :: IO ()
main = pure ()

data D
  = C1
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9

test :: Bar.Foo -> Bool
test x = case x of
           Bar.Foo -> True
           Bar.Bar -> True
           Bar.Baz -> True
