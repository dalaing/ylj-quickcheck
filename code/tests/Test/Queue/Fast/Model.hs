module Test.Queue.Fast.Model (
    tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Queue.Fast            as F
import qualified Queue.List            as L
import           Test.Queue.Model

tests :: TestTree
tests = testGroup "Model" [
          testCase "empty" emptyAssertion
        , testProperty "isEmpty" isEmptyProperty
        , testProperty "peek" peekProperty
        , testProperty "add" addProperty
        , testProperty "remove" removeProperty
        ]

emptyAssertion :: Assertion
emptyAssertion =
  F.toList F.empty @?= L.toList (L.empty :: L.Queue Int)

-- isEmptyC == isEmpty . abstr
isEmptyProperty :: F.Queue Int
                -> Bool
isEmptyProperty = commutesR F.toList F.isEmpty L.isEmpty

-- peekC == peek . abstr
peekProperty :: F.Queue Int
             -> Property
peekProperty q = 
  (not . F.isEmpty) q ==>
    commutesR F.toList F.peek L.peek q

-- abstr . addC x == add x . abstr
addProperty :: Int
            -> F.Queue Int
            -> Bool
addProperty x = commutes F.toList (F.add x) (L.add x)

-- abstr . removeC == remove . abstr
removeProperty :: F.Queue Int
               -> Property
removeProperty q = 
  (not . F.isEmpty) q ==>
    commutes F.toList F.remove L.remove q
