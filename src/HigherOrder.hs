{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module HigherOrder (
  type (<:)(..)
  , inj'
  , mask
  , mask'
  , hup
  ) where

import Lib 
import Prelude hiding (sum)

data f :<->: g = Iso { to :: f ->: g, from :: g ->: f }
{- which satisifies
     to . from = id
     from . to = id -}

sum :: (f a -> b) -> (g a -> b) -> (f + g) a -> b
sum f _ (L x) = f x
sum _ g (R x) = g x

isoRefl :: f :<->: f
isoRefl = Iso id id

isoSym :: f :<->: g -> g :<->: f
isoSym i = Iso (from i) (to i)

isoTrans :: f :<->: g -> g :<->: h -> f :<->: h
isoTrans i1 i2 = Iso (to i2 . to i1) (from i1 . from i2)

isoSumCong :: f :<->: f' -> g :<->: g' -> (f + g) :<->: (f' + g')
isoSumCong i1 i2 = Iso
  (sum (L . to i1) (R . to i2))
  (sum (L . from i1) (R . from i2))

isoSumComm :: (f + g) :<->: (g + f)
isoSumComm = Iso
  (sum R L)
  (sum R L)

isoSumAssoc :: (f + (g + h)) :<->: ((f + g) + h)
isoSumAssoc = Iso
  (sum (L . L) (sum (L . R) R))
  (sum (sum L (R . L)) (R . R))

data Forephism f g
  = forall f'. (Functor g, Functor f, Functor f') =>
      Forephism { iso :: g :<->: (f + f') }

infixr 4 <:
class (Functor f, Functor g) => f <: g where
  forephism :: Forephism f g

inj' :: f <: g => f a -> g a
inj' = case forephism of
  Forephism i -> from i . L

data End k -- No constructors!
  deriving Functor

instance Functor f => f <: f where
  forephism = Forephism (Iso
    L
    (sum id (\(x :: End a) -> case x of)))

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f <: f + g where
  forephism = Forephism isoRefl

instance (Functor f, Functor g, Functor g', f <: g')
      => f <: g + g' where
  forephism = case forephism of
    Forephism i -> Forephism
      (isoTrans
         (isoSumCong isoRefl i)
         (isoTrans isoSumComm (isoSym isoSumAssoc)))

mask :: Functor f => Free f a -> Free (f' + f) a
mask = fold Pure (Op . R)

mask' :: Functor f => Free f a -> Free (f + f') a
mask' = fold Pure (Op . L)

hup :: f <: g => (forall f'. Functor f' => Free (f + f') a -> Free f' b)
    -> Free g a -> Free g b
hup h = case forephism of
  Forephism i -> permute (from i) . mask . h . permute (to i)

data Ask r k
  = Ask (r -> k)
  deriving Functor

ask :: Ask r <: f => Free f r
ask = Op (inj' (Ask Pure))

hAsk :: Functor f' => r -> Handler (Ask r) a f' a
hAsk r = Handler
  { ret = pure
  , hdlr = \x -> case x of Ask k -> k r }

local :: (Functor f, Ask r <: f) => (r -> r) -> Free f a -> Free f a
local f m = do
  r <- ask
  hup (handle (hAsk (f r))) m


