module Main where

main :: IO ()
main = pure ()

test :: Maybe Bool -> Bool
test x = case x of
  SPLIT -> True
