{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Conc (
  par,
  goesFirst,
  (~||~),
  (~||>~)
  ) where

import Lib
import HigherOrder
import Choose
import Data.Tuple (swap)

-- function for running two programs concurrently (||)
par :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
par (Pure x) y = fmap (x,) y
par x (Pure y) = fmap (,y) x
par x y = do
  goesFirst x y ~+~ fmap swap (goesFirst y x)

-- function for running two programs concurrently, with the first one having priority (|L)
goesFirst :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
goesFirst (Pure x) y = fmap (x,) y
goesFirst (Op x) y = Op (fmap (`par` y) x)

-- Using the same symbol as in the book "Modelling and analysis of communicating systems" by Jan Friso Groote and Mohammad Reza Mousavi
(~||~) :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
x ~||~ y = par x y

-- Adaptation of the symbol for leftmerge from the book "Modelling and analysis of communicating systems" by Jan Friso Groote and Mohammad Reza Mousavi
(~||>~) :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
x ~||>~ y = goesFirst x y
