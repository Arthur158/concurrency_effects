{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

-- Module that handles nondeterminism
module Choose (
  Choose(..)
  , choose
  , zero
  , hChooseTrue
  , hChoose'
  , (~+~)
  , pick
  , sumContinuations 
  ) where

import Lib
import HigherOrder
import Control.Applicative
import Control.Monad

-- Effect for nondeterminism. Choose allows for a branching into a "True" branch and/or a "False" branch.
-- Pick is the same as choose but for a range of numbers
-- Zero is denoting an empty result.
data Choose k 
  = Choose (Bool -> k)
  | Pick Int (Int -> k)
  | Zero
  deriving Functor

-- Smart constructor for Choose
choose :: Choose <: f => Free f Bool
choose = Op (inj' (Choose Pure))

-- Smart constructor for Pick
pick :: Choose <: f => Int -> Free f Int
pick a = Op (inj' (Pick a Pure))

-- Smart constructor for Zero
zero :: Choose <: f => Free f a
zero = Op (inj' Zero)

-- Handler that always chooses the "True" or the "0" branch (No nondeterminism then)
hChooseTrue :: Functor f' => Handler Choose a f' (Maybe a)
hChooseTrue = Handler
  { ret = pure . Just
  , hdlr = \case 
    Choose f -> f True
    Pick _ f -> f 0
    Zero -> pure Nothing} -- here somehow feed f True or/and false

-- Nondeterministic handler for Choose. Accumulates the results of all the branches in a list
hChoose' :: Functor f' => Handler Choose a f' [a]
hChoose' = Handler
  { ret = \x -> pure [x]
  , hdlr = \case
               Choose f -> f False >>= \l -> fmap (++ l) (f True)
               Pick n f -> foldM (\acc i -> fmap (++ acc) (f i)) [] [0..n-1]
               Zero -> pure []}

-- Syntactic sugar for choosing between two continuations using Choose
(~+~) :: Choose <: f => Free f a -> Free f a -> Free f a
m1 ~+~ m2 = do
  b <- choose
  if b then m1 else m2

-- Syntactic sugar for choosing from a list of continuations using Choose
sumContinuations :: Choose <: f => [Free f a] -> Free f a
sumContinuations xs = do
  m <- pick (length xs)
  xs !! m

-- Using the Choose effect to make an instance of Alternative for Free f
instance (Functor f, Choose <: f) => Alternative (Free f) where
  empty = zero
  (<|>) = (~+~) 

