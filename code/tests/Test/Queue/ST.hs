module Test.Queue.ST (
    tests
  ) where

import Test.Tasty

import qualified Test.Queue.ST.Algebraic as A
import qualified Test.Queue.ST.Model as M

tests :: TestTree
tests = testGroup "ST" [
        A.tests
      , M.tests
      ]
