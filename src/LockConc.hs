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
  , hLock
  , lockPar
  , goesFirstLock
  , dlockPar
  , dgoesFirstLock
  ) where

import Lib
import HigherOrder
import Choose
import Data.Tuple (swap)

-- Effect for adding locks to programs
data Lock k = Lock  k | Unlock k
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

-- handle used to integrate Choose as part of f
fakeHandle :: Lock <: f' => Handler Lock a f' a
fakeHandle = Handler
  { ret = \x -> pure x
  , hdlr = \case
               Lock f -> Op (inj' (Lock f))
               Unlock f -> Op (inj' (Unlock f))}

lockPar :: (Choose <: f, Lock <: f) => Free (Lock + f) a -> Free (Lock + f) b -> Free f (a, b)
lockPar x y = handle fakeHandle (lPar x y)

-- version of par (from src/Conc.hs) that implements locks
lPar :: Choose <: f => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
lPar (Pure x) y = fmap (x,) y
lPar x (Pure y) = fmap (,y) x
lPar x y = goesFirstLock x y ~+~ fmap swap (goesFirstLock y x)

-- version of goesFirst (from src/Conc.hs) that implements locks
goesFirstLock :: Choose <: f => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
goesFirstLock (Pure x) y = lPar (pure x) y
goesFirstLock (Op f) y = case f of
                       R f' -> Op (inj' (fmap (`lPar` y) f'))
                       L f' -> case f' of
                                Lock k -> Op (inj' (Lock (lockFirst k y)))
                                Unlock k -> goesFirstLock k y

-- function called when a par is locked on a program
lockFirst :: Choose <: f => Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
lockFirst (Pure x) y = fmap (x,) y 
lockFirst (Op f) y = case f of
                       R f' -> Op (inj' (fmap (`lockFirst` y) f'))
                       L f' -> case f' of
                                Lock k -> Op (inj' (Lock (lockFirst k y)))
                                Unlock k -> Op (inj' (Unlock (lPar k y)))

-- version of lockPar where you can specify a maximum depth, to experiment with models such as the ABP model.
dlockPar :: (Lock <: f, Choose <: f) => Int -> Free (Lock + f) a -> Free (Lock + f) b -> Free f (a, b)
dlockPar n x y = handle fakeHandle (dlpar n x y)

dlpar :: Choose <: f => Int -> Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
dlpar 0 _ _ = zero
dlpar _ (Pure x) y = fmap (x,) y
dlpar _ x (Pure y) = fmap (,y) x
dlpar n x y = dgoesFirstLock n x y ~+~ fmap swap (dgoesFirstLock n y x)

dgoesFirstLock :: Choose <: f => Int -> Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
dgoesFirstLock n (Pure x) y = dlpar n (pure x) y
dgoesFirstLock n (Op f) y = case f of
                       R f' -> Op (inj' (fmap (flip (dlpar (n-1)) y) f'))
                       L f' -> case f' of
                                Lock k -> Op (inj' (Lock (dlockFirst n k y)))
                                Unlock k -> dgoesFirstLock n k y

dlockFirst :: Choose <: f => Int -> Free (Lock + f) a -> Free (Lock + f) b -> Free (Lock + f) (a, b)
dlockFirst _ (Pure x) y = fmap (x,) y 
dlockFirst n (Op f) y = case f of
                       R f' -> Op (inj' (fmap (flip (dlockFirst n) y) f'))
                       L f' -> case f' of
                                Lock k -> Op (inj' (Lock (dlockFirst n k y)))
                                Unlock k -> Op (inj' (Unlock (dlpar (n-1) k y)))
