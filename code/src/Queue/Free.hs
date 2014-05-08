{-# LANGUAGE DeriveFunctor     #-}
module Queue.Free (
    QueueF(..)
  , Queue
  , empty
  , isEmpty
  , peek
  , remove
  , add
  ) where

import           Control.Monad.Free

data QueueF a k = Empty k
                | IsEmpty (Bool -> k)
                | Peek (a -> k)
                | Remove k
                | Add a k
                deriving (Functor)

instance Show a => Show (QueueF a k) where
  show (Empty _)   = "Empty <next>"
  show (IsEmpty _) = "IsEmpty <Bool -> next>"
  show (Peek _)    = "Peek <a -> next>"
  show (Remove _)  = "Remove <next>"
  show (Add x _)   = "Add " ++ show x ++ " <next>"

type Queue a r = Free (QueueF a) r

empty :: Queue a ()
empty = liftF $ Empty ()

isEmpty :: Queue a Bool
isEmpty = liftF $ IsEmpty id

peek :: Queue a a
peek = liftF $ Peek id

remove :: Queue a ()
remove = liftF $ Remove ()

add :: a
    -> Queue a ()
add x = liftF $ Add x ()
