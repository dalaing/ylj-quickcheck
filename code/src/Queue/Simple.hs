module Queue.Simple (
    Queue()
  , empty
  , isEmpty
  , peek
  , remove
  , add
  , toList
  , equiv
  ) where

import           Control.Monad   (liftM, liftM2)
import           Data.Function   (on)
import           Test.QuickCheck (Arbitrary (..), frequency)

data Queue a = Queue [a] [a]
                   deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a 
        -> Bool
isEmpty (Queue xs ys) = null xs && null ys

peek :: Queue a 
     -> a
peek (Queue (x : _) _) = x
peek _ = error "peek: empty queue"

remove :: Queue a 
       -> Queue a
remove (Queue (_ : xs) ys) = mkValid xs ys
remove _ = error "remove: empty queue"

add :: a 
    -> Queue a 
    -> Queue a
add x (Queue xs ys) = mkValid xs (x : ys)

toList :: Queue a 
       -> [a]
toList (Queue xs ys) = xs ++ reverse ys

equiv :: Eq a
      => Queue a
      -> Queue a
      -> Bool
equiv = (==) `on` toList

mkValid :: [a]
        -> [a]
        -> Queue a
mkValid [] ys = Queue (reverse ys) []
mkValid xs ys = Queue xs ys

removeIfNotEmpty :: Queue a
                 -> Queue a
removeIfNotEmpty q
  | isEmpty q = q
  | otherwise = remove q

instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = frequency [
              (1, return empty)
            , (2, liftM removeIfNotEmpty arbitrary)
            , (5, liftM2 add arbitrary arbitrary)
            ]
