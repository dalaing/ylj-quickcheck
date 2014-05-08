module Test.Queue.Simple.Model (
    tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Queue.List            as L
import qualified Queue.Simple          as S
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
  S.toList S.empty @?= L.toList (L.empty :: L.Queue Int)

-- isEmptyC == isEmpty . abstr
isEmptyProperty :: S.Queue Int
                -> Bool
isEmptyProperty = commutesR S.toList S.isEmpty L.isEmpty

-- peekC == peek . abstr
peekProperty :: S.Queue Int
             -> Property
peekProperty q = 
  (not . S.isEmpty) q ==>
    commutesR S.toList S.peek L.peek q

-- abstr . addC x == add x . abstr
addProperty :: Int
            -> S.Queue Int
            -> Bool
addProperty x = commutes S.toList (S.add x) (L.add x)

-- abstr . removeC == remove . abstr
removeProperty :: S.Queue Int
               -> Property
removeProperty q = 
  (not . S.isEmpty) q ==> 
    commutes S.toList S.remove L.remove q
