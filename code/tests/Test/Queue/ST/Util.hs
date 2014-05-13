{-# LANGUAGE RankNTypes #-}
module Test.Queue.ST.Util (
    Action
  , actions
  , delta
  , perform
  , equiv
  , equivEmpty
  , implements
  , implementsPre
  , implementsR
  , implementsPreR
  ) where

import qualified Queue.Free                 as F
import           Queue.ST                   hiding (equiv)

import           Control.Monad              (liftM, liftM2, void)
import           Control.Monad.Free
import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Writer
import           Data.STRef
import qualified Data.Vector                as V
import           Test.QuickCheck            (Arbitrary (..), Gen, Property,
                                             oneof)
import           Test.QuickCheck.Monadic

type Action a = F.Queue a ()

actions :: Arbitrary a
        => Int
        -> Gen [Action a]
actions n = oneof $ [
            return []
          , liftM2 (:)
             (liftM F.add arbitrary)
             (actions (n + 1))
          ] ++ if n == 0 then [] else [
            liftM (F.remove :) (actions (n - 1))
          , liftM (void F.peek :) (actions n)
          ]

delta :: [Action a] -> Int
delta = sum . map deltaOp
  where
    deltaOp d = case d of
      Free (F.Add _ _) -> 1
      Free (F.Remove _) -> -1
      _ -> 0

perform :: Queue s a
        -> [Action a]
        -> ST s [a]
perform sq = execWriterT . mapM_ step
  where
    step (Free (F.Empty _)) =
      lift . writeSTRef sq $ V.empty
    step (Free (F.IsEmpty _)) =
      void . lift . isEmpty $ sq
    step (Free (F.Peek _)) = do
      x <- lift . peek $ sq
      tell [x]
    step (Free (F.Add x _)) =
      lift . add x $ sq
    step (Free (F.Remove _)) =
      lift . remove $ sq
    step (Pure _) = return ()

observe :: [Action a] -> ST s [a]
observe acts = do
    q <- empty
    perform q acts

equiv :: ( Arbitrary a
         , Eq a
         , Show a
         )
      => [Action a]
      -> [Action a]
      -> Property
equiv c c' = monadicST $ do
  prefix <- pick $ actions 0
  suffix <- pick $ actions (delta (prefix ++ c))

  equivalent <- run $ liftM2 (==)
    (observe (prefix ++ c ++ suffix))
    (observe (prefix ++ c' ++ suffix))

  assert equivalent

equivEmpty :: ( Arbitrary a
              , Eq a
              , Show a
              )
           => [Action a]
           -> [Action a]
           -> Property
equivEmpty c c' = monadicST $ do
  suffix <- pick $ actions (delta c)

  equivalent <- run $ liftM2 (==)
    (observe (c ++ suffix))
    (observe (c' ++ suffix))

  assert equivalent

-- abstr . l = r . abstr
commutes :: Eq a
         => Queue s a
         -> (Queue s a -> ST s ())
         -> ([a] -> [a])
         -> ST s Bool
commutes sq l r = do
    old <- toList sq
    l sq
    newer <- toList sq
    return $ r old == newer

initializeQueue :: (Arbitrary a, Show a, Eq a) 
                => PropertyM (ST s) (Queue s a)
initializeQueue = do
    as <- pick $ actions 0
    q <- run empty
    run . void $ perform q as

    return q

implements :: (Arbitrary a, Show a, Eq a)
           => (forall s. Queue s a -> ST s ())
           -> ([a] -> [a])
           -> Property
implements l r = monadicST $ do
    q <- initializeQueue
    assert =<< run (commutes q l r)

implementsPre :: (Arbitrary a, Show a, Eq a)
              => (forall s. Queue s a -> ST s Bool)
              -> (forall s. Queue s a -> ST s ())
              -> ([a] -> [a])
              -> Property
implementsPre p l r = monadicST $ do
    q <- initializeQueue
    pre =<< run (p q)
    assert =<< run (commutes q l r)

-- l = r . abstr
commutesR :: Eq b
          => Queue s a
          -> (Queue s a -> ST s b)
          -> ([a] -> b)
          -> ST s Bool
commutesR sq l r = do
    old <- toList sq
    new <- l sq
    return $ r old == new

implementsR :: (Arbitrary a, Show a, Eq a, Eq b)
         => (forall s. Queue s a -> ST s b)
         -> ([a] -> b)
         -> Property
implementsR l r = monadicST $ do
  q <- initializeQueue
  assert =<< run (commutesR q l r)

implementsPreR :: (Arbitrary a, Show a, Eq a, Eq b)
               => (forall s. Queue s a -> ST s Bool)
               -> (forall s. Queue s a -> ST s b)
               -> ([a] -> b)
               -> Property
implementsPreR p l r = monadicST $ do
  q <- initializeQueue
  pre =<< run (p q)
  assert =<< run (commutesR q l r)

