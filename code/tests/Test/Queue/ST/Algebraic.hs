module Test.Queue.ST.Algebraic (
    tests
  ) where

import Test.Queue.ST.Util

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Queue.Free as F
import Control.Monad (void)

tests :: TestTree
tests = testGroup "Algebraic" [
        emptyTests
      , nonEmptyTests
      ]

emptyTests :: TestTree
emptyTests = testGroup "Empty" [
             testProperty "add-remove" addRemoveEmptyProp
           ]

addRemoveEmptyProp :: Int 
              -> Property
addRemoveEmptyProp m = equivEmpty
  [F.add m, F.remove]
  []

nonEmptyTests :: TestTree
nonEmptyTests = testGroup "Non-empty" [
                testProperty "add-peek" addPeekProp
              , testProperty "add-remove" addRemoveProp
              ]

addPeekProp :: Int 
            -> Int 
            -> Property
addPeekProp m n = equiv 
  [F.add m, F.add n, void F.peek]
  [F.add m, void F.peek, F.add n]

addRemoveProp :: Int 
              -> Int 
              -> Property
addRemoveProp m n = equiv
  [F.add m, F.add n, F.remove]
  [F.add m, F.remove, F.add n]

