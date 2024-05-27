
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
  , unToND
  ) where

import Lib
import NonDet
import HigherOrder
import Err


data End k -- No constructors!
  deriving Functor

un :: Free End a -> a
un (Pure x) = x
un (Op f) = case f of

unToND :: [FreeND End a] -> [a]
unToND [] = []
unToND (PureND x : xs) = x : unToND xs
unToND (OpND f : xs) = [case f of]

