{-# LANGUAGE TypeSynonymInstances #-}
module Queue.List (
    Queue()
  , empty
  , isEmpty
  , peek
  , remove
  , add
  , toList
  , equiv
  ) where

type Queue a = [a]

empty :: Queue a
empty = []

isEmpty :: Queue a
        -> Bool
isEmpty = null

peek :: Queue a
     -> a
peek = head

remove :: Queue a
       -> Queue a
remove = tail

add :: a
    -> Queue a
    -> Queue a
add x xs = xs ++ [x]

toList :: Queue a
       -> [a]
toList = id

equiv :: Eq a
      => Queue a
      -> Queue a
      -> Bool
equiv = (==)

