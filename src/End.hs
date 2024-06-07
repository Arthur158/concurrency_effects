
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

module End (
  End
  , un
  ) where

import Lib
import HigherOrder
import Err


data End k -- No constructors!
  deriving Functor

un :: Free End a -> a
un (Pure x) = x
un (Op f) = case f of
