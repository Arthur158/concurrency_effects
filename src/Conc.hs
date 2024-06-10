
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Conc (
  par,
  fpar,
  goesFirst,
  dPar,
  dGoesFirst,
  ) where

import Lib
import HigherOrder
import Choose
import Data.Tuple (swap)


fakeHandle :: Choose <: f' => Handler Choose a f' a
fakeHandle = Handler
  { ret = \x -> pure x
  , hdlr = \case
               Choose f -> Op (inj' (Choose f))
               Zero -> Op (inj' Zero)}

fpar :: (Functor f, Choose <: f) => Free (Choose + f) a -> Free (Choose + f) b -> Free f (a, b)
fpar x y = handle fakeHandle (par x y)

-- function for running two programs concurrently (||)
par :: (Functor f) => Free (Choose + f) a -> Free (Choose + f) b -> Free (Choose + f) (a, b)
par (Pure x) y = fmap (x,) y
par x (Pure y) = fmap (,y) x
par x y = do
  goesFirst x y ~+~ fmap swap (goesFirst y x)

-- function for running two programs concurrently, with the first one having priority (|L)
goesFirst :: (Functor f) => Free (Choose + f) a -> Free (Choose + f) b -> Free (Choose + f) (a, b)
goesFirst (Pure x) y = fmap (x,) y
goesFirst (Op x) y = case x of 
                       L f -> Op (inj' (fmap (`goesFirst` y) f))
                       R _ -> Op (fmap (`par` y) x)

-- -- Using the same symbol as in the book "Modelling and analysis of communicating systems" by Jan Friso Groote and Mohammad Reza Mousavi
-- (~||~) :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- x ~||~ y = par x y
--
-- -- Adaptation of the symbol for leftmerge from the book "Modelling and analysis of communicating systems" by Jan Friso Groote and Mohammad Reza Mousavi
-- (~||>~) :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- x ~||>~ y = goesFirst x y

-- function for running two programs concurrently (||)
dPar :: Choose <: f => Int -> Free f a -> Free f b -> Free f (a, b)
dPar 0 _ _ = zero 
dPar _ (Pure x) y = fmap (x,) y
dPar _ x (Pure y) = fmap (,y) x
dPar n x y = do
  dGoesFirst  n x y ~+~ fmap swap (dGoesFirst n y x)

-- function for running two programs concurrently, with the first one having priority (|L)
dGoesFirst :: Choose <: f => Int -> Free f a -> Free f b -> Free f (a, b)
dGoesFirst _ (Pure x) y = fmap (x,) y
dGoesFirst n (Op x) y = Op (fmap (flip (dPar (n - 1)) y) x)
