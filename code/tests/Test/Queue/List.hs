module Test.Queue.List (
    tests
  ) where

import Test.Tasty

import qualified Test.Queue.List.Algebraic as A

tests :: TestTree
tests = testGroup "List" [
        A.tests
      ]
