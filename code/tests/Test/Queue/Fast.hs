module Test.Queue.Fast (
    tests
  ) where

import Test.Tasty

import qualified Test.Queue.Fast.Algebraic as A
import qualified Test.Queue.Fast.Model as M

tests :: TestTree
tests = testGroup "Fast" [
        A.tests
      , M.tests
      ]
