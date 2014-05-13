module Test.Queue (
    tests
  ) where

import Test.Tasty

import qualified Test.Queue.List as L
import qualified Test.Queue.Simple as S
import qualified Test.Queue.Fast as F
import qualified Test.Queue.ST as ST

tests :: TestTree
tests = testGroup "Queue" [
        L.tests
      , S.tests
      , F.tests
      , ST.tests
      ]
