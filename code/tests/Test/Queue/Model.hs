module Test.Queue.Model (
    commutes
  , commutesR
  ) where

import Control.Applicative (liftA2)

commutes :: Eq b 
         => (a -> b) 
         -> (a -> a) 
         -> (b -> b) 
         -> a
         -> Bool 
commutes norm l r = 
  liftA2 (==) (norm . l) (r . norm)

commutesR :: Eq c
          => (a -> b)
          -> (a -> c) 
          -> (b -> c) 
          -> a
          -> Bool
commutesR norm l r =
  liftA2 (==) l (r . norm)
