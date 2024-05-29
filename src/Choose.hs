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
  , Free (..)
  , pick
  , sumContinuations 
  , hChooseLog
  , hChooseOrder
  ) where

import Lib
import HigherOrder
import Control.Applicative
import Control.Monad

-- Effect for nondeterminism. Choose allows for a branching into a "True" branch and/or a "False" branch.
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
    Zero -> pure Nothing}

-- Handler that makes a list accumulating the results of the "True" and "False" branches. Zeroes are discarded.
hChoose' :: Functor f' => Handler Choose a f' [a]
hChoose' = Handler
  { ret = \x -> pure [x]
  , hdlr = \case
               Choose f -> f False >>= \l -> fmap (++ l) (f True)
               Pick n f -> foldM (\acc i -> fmap (++ acc) (f i)) [] [0..n-1]
               Zero -> pure []}

-- Function for choosing nondeterministically between two continuation
(~+~) :: Choose <: f => Free f a -> Free f a -> Free f a
m1 ~+~ m2 = do
  b <- choose
  if b then m1 else m2

sumContinuations :: Choose <: f => [Free f a] -> Free f a
sumContinuations xs = do
  m <- pick (length xs)
  xs !! m

-- Using the Choose effect to make an instance of Alternative for Free f
instance (Functor f, Choose <: f) => Alternative (Free f) where
  empty = zero
  (<|>) = (~+~) 

-- Handler that makes a list accumulating the results of the "True" and "False" branches. Zeroes are discarded.
hChooseLog :: Functor f' => Handler Choose a f' [(a, [Bool])]
hChooseLog = Handler
  { ret = \x -> pure [(x, [])]
  , hdlr = \case
               Choose f -> fmap (map (\(x,ls) -> (x,ls ++ [False]))) (f False) >>= \l -> fmap (++ l) (fmap (map (\(x,ls) -> (x,ls ++ [True]))) (f True))
               Pick n f -> foldM (\acc i -> fmap (++ acc) (f i)) [] [0..n-1]
               Zero -> pure []}

hChooseOrder :: Handler_ Choose a [Bool] g (Either String a)
hChooseOrder = Handler_
  { ret_ = \x _ -> Pure (Right x)
    , hdlr_ = \x ss -> case (x, ss) of
                        (Choose f, h:t) -> f h (t ++ [h])
                        (Choose _, []) -> Pure (Left "The list given was empty")
                        (Pick _ _, _) -> Pure (Left "Pick is not supported by this handler")
                        (Zero, _) -> Pure (Left "The continuation ended in a Zero")}
