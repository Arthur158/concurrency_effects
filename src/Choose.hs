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
  , hChooseLog
  , hChooseOrder
  ) where

import Lib
import HigherOrder
import Control.Applicative
import Err

-- Effect for nondeterminism. Choose allows for a branching into a "True" branch and/or a "False" branch.
-- Zero is denoting an empty result.
data Choose k 
  = Choose (Bool -> k)
  | Zero
  deriving Functor

-- Smart constructor for Choose
choose :: Choose <: f => Free f Bool
choose = Op (inj' (Choose Pure))

-- Smart constructor for Zero
zero :: Choose <: f => Free f a
zero = Op (inj' Zero)

-- Handler that always chooses the "True" or the "0" branch (No nondeterminism then)
hChooseTrue :: Functor f' => Handler Choose a f' (Maybe a)
hChooseTrue = Handler
  { ret = pure . Just
  , hdlr = \case 
    Choose f -> f True
    Zero -> pure Nothing}

-- Handler that makes a list accumulating the results of the "True" and "False" branches. Zeroes are discarded.
hChoose' :: Functor f' => Handler Choose a f' [a]
hChoose' = Handler
  { ret = \x -> pure [x]
  , hdlr = \case
               Choose f -> f False >>= \l -> fmap (++ l) (f True)
               Zero -> pure []}

-- Function for choosing nondeterministically between two continuation
(~+~) :: Choose <: f => Free f a -> Free f a -> Free f a
m1 ~+~ m2 = do
  b <- choose
  if b then m1 else m2

-- Using the Choose effect to make an instance of Alternative for Free f
instance (Functor f, Choose <: f) => Alternative (Free f) where
  empty = zero
  (<|>) = (~+~) 

-- Handler that makes a list accumulating the results of the "True" and "False" branches. Zeroes are discarded.
hChooseLog :: Functor f' => Handler Choose a f' [(a, [Bool])]
hChooseLog = Handler
  { ret = \x -> pure [(x, [])]
  , hdlr = \case
               Choose f -> fmap (map (\(x,ls) -> (x,[False] ++ ls))) (f False) >>= \l -> fmap (++ l) (fmap (map (\(x,ls) -> (x,True:ls))) (f True))
               Zero -> pure []}

-- Deterministic handler that takes a list of Bool to specify which branch of the Choose to choose.
hChooseOrder :: (Err <: g) => Handler_ Choose a [Bool] g a
hChooseOrder = Handler_
  { ret_ = \x _ -> Pure x
    , hdlr_ = \x ss -> case (x, ss) of
                        (Choose f, h:t) -> f h (t ++ [h])
                        (Choose _, []) -> err' "The list given was empty"
                        (Zero, _) -> err' "The continuation ended in a Zero"}
