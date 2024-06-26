{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}

module State (
  State(..)         
  , get            
  , get'          
  , put          
  , put'        
  , hErr       
  , hState'
  , hStateS
  ) where

import Lib
import HigherOrder
import Err
import End

data State s k
  = Put s k
  | Get (s -> k)
  deriving Functor

get :: State s < f => Free f s
get = Op (inj (Get Pure))

put  :: State s < f => s -> Free f ()
put s = Op (inj (Put s (Pure ())))

hState :: Functor g => Handler_ (State s) a s g (a, s)
hState = Handler_
  { ret_ = \x s -> pure (x, s)
  , hdlr_ = \x s -> case x of
      Put s' k -> k s'
      Get k -> k s s }

hStateS :: Functor g => Handler_ (State s) a s g a
hStateS = Handler_
  { ret_ = \x s -> pure x
  , hdlr_ = \x s -> case x of
      Put s' k -> k s'
      Get k -> k s s }

hState' :: Functor g => Handler_ (State s) a [s] g (a, [s])
hState' = Handler_
  { ret_ = \x ss -> pure (x, ss)
  , hdlr_ = \x ss -> case x of
      Put s' k -> k (s':ss)
      Get k -> k (head ss) ss }

get' :: State s <: f => Free f s
get' = Op (inj' (Get Pure))

put'  :: State s <: f => s -> Free f ()
put' s = Op (inj' (Put s (Pure ())))
