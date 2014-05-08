module Test.Queue.Simple.Algebraic (
    tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Queue.Simple

tests :: TestTree
tests = testGroup "Algebraic" [
        emptyTests
      , peekTests
      , removeTests
      ]

emptyTests :: TestTree
emptyTests = testGroup "Empty" [
             testCase "empty" emptyAssert
           , testProperty "non-empty" nonEmptyProp
           ]

emptyAssert :: Assertion
emptyAssert =
  isEmpty empty @?= True

nonEmptyProp :: Int
             -> Queue Int
             -> Bool
nonEmptyProp x =
  not . isEmpty . add x

peekTests :: TestTree
peekTests = testGroup "peek" [
             testProperty "peekAddEmpty" peekAddEmptyProp
           , testProperty "peekAddNonEmpty" peekAddNonEmptyProp
           ]

peekAddEmptyProp :: Int
                 -> Bool
peekAddEmptyProp x =
  peek (add x empty) == x

peekAddNonEmptyProp :: Int
                    -> Queue Int
                    -> Property
peekAddNonEmptyProp x xs =
  (not . isEmpty) xs ==>
    peek (add x xs) == peek xs

removeTests :: TestTree
removeTests = testGroup "remove" [
            testProperty "removeAddEmpty" removeAddEmptyProp
          , testProperty "removeAddNonEmpty" removeAddNonEmptyProp
          ]

removeAddEmptyProp :: Int
                   -> Bool
removeAddEmptyProp x =
  remove (add x empty) == empty

removeAddNonEmptyProp :: Int
                      -> Queue Int
                      -> Property
removeAddNonEmptyProp x xs =
  (not . isEmpty) xs ==>
    remove (add x xs) `equiv` add x (remove xs)
