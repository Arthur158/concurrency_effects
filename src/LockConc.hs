{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Tuple (swap)

-- Effect for adding locks to programs
data Lock k = Lock  k| Unlock k
  deriving Functor

--smart constructor for lock
lock :: Lock <: f => Free f ()
lock = Op (inj' (Lock (Pure ())))

--smart constructor for unlock
unlock :: Lock <: f => Free f ()
unlock = Op (inj' (Unlock (Pure ())))

-- The handler is simply applied in order to remove any remaining lock when the program has already been interleaved
hLock :: Functor f' => Handler Lock a f' a
hLock = Handler
  { ret = pure
    , hdlr = \case
                   Lock k -> k
                   Unlock k -> k}

-- version of par (from src/Conc.hs) that implements locks
lockPar :: (Choose <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
lockPar (Pure x) y = fmap (x,) y
lockPar x (Pure y) = fmap (,y) x
lockPar x y = goesFirstLock x y ~+~ fmap swap (goesFirstLock y x)

-- version of goesFirst (from src/Conc.hs) that implements locks
goesFirstLock :: (Choose <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
goesFirstLock (Pure x) y = lockPar (pure x) y
goesFirstLock (Op f) y = case f of
                       R _ -> Op (inj' (fmap (`lockPar` y) f))
                       L f' -> case f' of
                                Lock k -> Op (inj' (Lock (lockFirst k y)))
                                Unlock k -> Op (inj' (Unlock (lockPar k y)))

-- function called when a par is locked on a program
lockFirst :: (Choose <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
lockFirst (Pure x) y = fmap (x,) y 
lockFirst (Op f) y = case f of
                       R _ -> Op (inj' (fmap (`lockFirst` y) f))
                       L f' -> case f' of
                                Lock k -> Op (inj' (Lock (lockFirst k y)))
                                Unlock k -> Op (inj' (Unlock (lockPar k y)))
