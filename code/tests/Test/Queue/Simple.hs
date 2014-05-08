module Test.Queue.Simple (
    tests
  ) where

import Test.Tasty

import qualified Test.Queue.Simple.Algebraic as A
import qualified Test.Queue.Simple.Model as M

tests :: TestTree
tests = testGroup "Simple" [
        A.tests
      , M.tests
      ]
