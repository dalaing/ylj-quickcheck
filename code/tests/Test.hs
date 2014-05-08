module Main where

import Test.Tasty

import qualified Test.Queue as Q

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
        Q.tests
      ]
