
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverlappingInstances #-}


module Err (
  Err(..)
  , err
  , err'
  , hErr
) where

import Lib
import HigherOrder

data Err k = Err String
  deriving Functor

err :: Err < f => String -> Free f a
err msg = Op (inj (Err msg))

err' :: Err <: f => String -> Free f a
err' msg = Op (inj' (Err msg))


hErr :: Functor f' => Handler Err a f' (Either String a)
hErr = Handler
  { ret = pure . Right
  , hdlr = \x -> case x of Err s -> pure (Left s) }
