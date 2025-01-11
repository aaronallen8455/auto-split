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
    ]
  , testGroup "lambda case"
    [ testCase "1" $ runTest "LambdaCase1.hs"
    ]
  , testGroup "lambda cases"
    [ testCase "1" $ runTest "LambdaCases1.hs"
    , testCase "2" $ runTest "LambdaCases2.hs"
    ]
  , testGroup "fun cases"
    [ testCase "1" $ runTest "Fun1.hs"
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
