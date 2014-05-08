module Queue.ST (
    Queue
  , empty
  , isEmpty
  , peek
  , remove
  , add
  , toList
  , equiv
  ) where

import           Control.Monad    (liftM2)
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector      as V

type Queue s a = STRef s (V.Vector a)

empty :: ST s (Queue s a)
empty = newSTRef V.empty

isEmpty :: Queue s a
        -> ST s Bool
isEmpty sq = do
  q <- readSTRef sq
  return (V.null q)

peek :: Queue s a
     -> ST s a
peek sq = do
  q <- readSTRef sq
  return $ V.head q

remove :: Queue s a
       -> ST s ()
remove sq = modifySTRef sq V.tail

add :: a
    -> Queue s a
    -> ST s ()
add x sq = modifySTRef sq (`V.snoc` x)

toList :: Queue s a
       -> ST s [a]
toList sq = do
  q <- readSTRef sq
  return . V.toList $ q

equiv :: Eq a
      => Queue s a
      -> Queue s a
      -> ST s Bool
equiv sq1 sq2 = liftM2 (==)
  (toList sq1)
  (toList sq2)
