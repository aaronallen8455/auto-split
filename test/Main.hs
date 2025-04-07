{-# LANGUAGE CPP #-}
module Main (main) where

import           Control.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified System.Directory as Dir
import qualified System.Process as Proc

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "case"
    [ testCase "1" $ runTest "Case1.hs"
    , testCase "2" $ runTest "Case2.hs"
    , testCase "3" $ runTest "Case3.hs"
    , testCase "4" $ runTest "Case4.hs"
    , testCase "5" $ runTest "Case5.hs"
    , testCase "6" $ runTest "Case6.hs"
    , testCase "7" $ runTest "Case7.hs"
    , testCase "8" $ runTest "Case8.hs"
    , testCase "9" $ runTest "Case9.hs"
    , testCase "10" $ runTest "Case10.hs"
    , testCase "11" $ runTest "Case11.hs"
    , testCase "13" $ runTest "Case13.hs"
#if __GLASGOW_HASKELL__ >= 912
    , testCase "12" $ runTest "Case12.hs"
    , testCase "14" $ runTest "Case14.hs"
    , testCase "15" $ runTest "Case15.hs"
    , testCase "16" $ runTest "Case16.hs"
#endif
    , testCase "17" $ runTest "Case17.hs"
    , testCase "18" $ runTest "Case18.hs"
    , testCase "19" $ runTest "Case19.hs"
    ]
  , testGroup "lambda case"
    [ testCase "1" $ runTest "LambdaCase1.hs"
    , testCase "2" $ runTest "LambdaCase2.hs"
    ]
  , testGroup "lambda cases"
    [ testCase "1" $ runTest "LambdaCases1.hs"
    , testCase "2" $ runTest "LambdaCases2.hs"
    , testCase "3" $ runTest "LambdaCases3.hs"
    , testCase "4" $ runTest "LambdaCases4.hs"
    ]
  , testGroup "fun cases"
    [ testCase "1" $ runTest "Fun1.hs"
    , testCase "2" $ runTest "Fun2.hs"
    , testCase "3" $ runTest "Fun3.hs"
    , testCase "4" $ runTest "Fun4.hs"
    , testCase "5" $ runTest "Fun5.hs"
    , testCase "6" $ runTest "Fun6.hs"
    ]
  , testGroup "auto fields"
    [ testCase "1" $ runTest "Fields1.hs"
    , testCase "2" $ runTest "Fields2.hs"
    , testCase "3" $ runTest "Fields3.hs"
    , testCase "4" $ runTest "Fields4.hs"
    ]
  ]

testModulePath :: String -> FilePath
testModulePath name = "test-modules/" <> name

-- copy the input file contents to the module file to be compiled
prepTest :: FilePath -> IO ()
prepTest modFile = do
  inp <- readFile (modFile ++ ".input")
  writeFile modFile inp

runTest :: FilePath -> Assertion
runTest name = do
  let modFile = testModulePath name
  prepTest modFile
  (_, _, _, h) <- Proc.createProcess $
    Proc.proc "cabal" ["build", "test-modules:" ++ takeWhile (/= '.') name]
  void $ Proc.waitForProcess h
  updatedMod <- readFile modFile
  expectedMod <- readFile $ modFile ++ ".expected"
  assertEqual "Expected update" expectedMod updatedMod
  Dir.removeFile modFile
