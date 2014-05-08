module Test.Queue.Fast.Algebraic (
    tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Queue.Fast

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
             testProperty "peekaddEmpty" peekaddEmptyProp
           , testProperty "peekaddNonEmpty" peekaddNonEmptyProp
           ]

peekaddEmptyProp :: Int 
                 -> Bool
peekaddEmptyProp x =
  peek (add x empty) == x

peekaddNonEmptyProp :: Int 
                    -> Queue Int 
                    -> Property
peekaddNonEmptyProp x xs =
  (not . isEmpty) xs ==>
    peek (add x xs) == peek xs

removeTests :: TestTree
removeTests = testGroup "remove" [
            testProperty "removeaddEmpty" removeaddEmptyProp
          , testProperty "removeaddNonEmpty" removeaddNonEmptyProp
          ]

removeaddEmptyProp :: Int 
                   -> Bool
removeaddEmptyProp x =
  remove (add x empty) == empty

removeaddNonEmptyProp :: Int 
                      -> Queue Int 
                      -> Property
removeaddNonEmptyProp x xs =
  (not . isEmpty) xs ==>
    remove (add x xs) `equiv` add x (remove xs)
