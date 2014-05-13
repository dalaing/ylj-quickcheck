module Test.Queue.ST.Model (
    tests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Queue.ST.Util

import Control.Monad.ST

import qualified Queue.List as L
import qualified Queue.ST as S

tests :: TestTree
tests = testGroup "Model" [
          testProperty "isEmpty" isEmptyProp
        , testProperty "add" addProp
        , testProperty "remove" removeProp
        , testProperty "peek" peekProp
        ]

isEmptyProp :: Property
isEmptyProp = implementsR 
                S.isEmpty 
                (L.isEmpty :: L.Queue Int -> Bool)

addProp :: Int -> Property
addProp n = implements (S.add n) (L.add n)

notEmpty :: S.Queue s a -> ST s Bool
notEmpty q = fmap not (S.isEmpty q)

removeProp :: Property
removeProp = implementsPre 
                notEmpty
                S.remove 
                (L.remove :: L.Queue Int -> L.Queue Int)

peekProp :: Property
peekProp = implementsPreR
             notEmpty
             S.peek
             (L.peek :: L.Queue Int -> Int)
