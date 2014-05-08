module Queue.Fast (
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

data Queue a = Queue [a] [a] [a]
                 deriving (Eq, Show)

empty :: Queue a
empty = Queue [] [] []

isEmpty :: Queue a 
        -> Bool
isEmpty (Queue xs _ _) = null xs

peek :: Queue a 
     -> a
peek (Queue (x : _) _ _) = x

remove :: Queue a 
       -> Queue a
remove (Queue (_ : xs) ys zs) = mkValid xs ys zs

add :: a 
    -> Queue a 
    -> Queue a
add x (Queue xs ys zs) = mkValid xs (x : ys) zs

toList :: Queue a 
       -> [a]
toList (Queue xs ys _) = xs ++ reverse ys

equiv :: Eq a
      => Queue a
      -> Queue a
      -> Bool
equiv = (==) `on` toList

mkValid :: [a]
        -> [a]
        -> [a]
        -> Queue a
mkValid xs ys [] = Queue zs [] zs
  where
    zs = rot xs ys []
mkValid xs ys (_ : zs) = Queue xs ys zs 

rot :: [a]
    -> [a]
    -> [a]
    -> [a]
rot [] [y] zs            = y : zs
rot (x : xs) (y : ys) zs = x : rot xs ys (y : zs)
rot _ _ _                = error "rot: should not happen"

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
