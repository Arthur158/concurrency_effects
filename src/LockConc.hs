{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE IncoherentInstances #-}

module LockConc (
  Lock(..)
  , lock
  , unlock
  , lockPar
  , hLock
  ) where

import Lib
import HigherOrder
import Choose
import Conc
import Data.Tuple (swap)

data Lock k = Lock  k| Unlock k
  deriving Functor

-- lock is a syntactic sugar effect that would get removed by applying the par function, as in, when 2 Free f a would get combined using par, the Locks would get removed.
-- currently, would simply lock the entire calculation, instead of just a single resourcec like a particular state or something

lock :: (Functor f, Lock <: f) => Free f ()
lock = Op (inj' (Lock (Pure ())))

unlock :: (Functor f, Lock <: f) => Free f ()
unlock = Op (inj' (Unlock (Pure ())))

-- The handler is simply applied in order to remove any remaining lock when the program has already been interleaved
hLock :: Functor f' => Handler Lock a f' a
hLock = Handler
  { ret = pure
    , hdlr = \x -> case x of
                   Lock k -> k
                   Unlock k -> k}

-- Don't forget to handle deadlock at some point, or double locking
lockPar :: (Choose <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
lockPar (Pure x) program2 = fmap (x,) program2
lockPar program1 (Pure y) = fmap (,y) program1
lockPar program1 program2 = goesFirstLock program1 program2 ~+~ fmap swap (goesFirstLock program2 program1)

goesFirstLock :: (Choose <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
goesFirstLock (Pure x) y = fmap (x,) y
goesFirstLock (Op f) y = case f of
                       R f -> Op (inj' (fmap (`lockPar` y) f))
                       L f -> case f of
                                Lock k -> Op (inj' (Lock (lockFirst k y)))
                                Unlock k -> Op (inj' (Unlock (lockPar k y)))

lockFirst :: (Choose <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
lockFirst (Pure x) y = fmap (x,) y 
lockFirst (Op f) y = case f of
                       R f -> Op (inj' (fmap (`lockFirst` y) f))
                       L f -> case f of
                                Lock k -> Op (inj' (Lock (lockFirst k y)))
                                Unlock k -> Op (inj' (Unlock (lockPar k y)))

