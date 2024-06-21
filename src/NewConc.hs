{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module NewConc (
  newpar,
  ) where

import Lib
import HigherOrder
import Choose
import Data.Tuple (swap)

-- handle used to integrate Choose as part of f
fakeHandle :: Choose <: f' => Handler Choose a f' a
fakeHandle = Handler
  { ret = \x -> pure x
  , hdlr = \case
               Choose f -> Op (inj' (Choose f))
               Zero -> Op (inj' Zero)}

newpar :: (Functor f, Choose <: f) => Free (Choose + f) a -> Free (Choose + f) b -> Free f (a, b)
newpar x y = handle fakeHandle (par x y)

-- version of par for respecting law LM4.
par :: (Functor f) => Free (Choose + f) a -> Free (Choose + f) b -> Free (Choose + f) (a, b)
par (Pure x) y = fmap (x,) y
par x (Pure y) = fmap (,y) x
par x y = do
  goesFirst x y ~+~ fmap swap (goesFirst y x)

-- version of goesFirst that matchmakes on the effect, to recursively call goesFirst in case of a Choose
goesFirst :: (Functor f) => Free (Choose + f) a -> Free (Choose + f) b -> Free (Choose + f) (a, b)
goesFirst (Pure x) y = fmap (x,) y
goesFirst (Op x) y = case x of 
                       L f -> Op (inj' (fmap (`goesFirst` y) f))
                       R _ -> Op (fmap (`par` y) x)
