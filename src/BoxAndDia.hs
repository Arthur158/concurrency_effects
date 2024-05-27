{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module BoxAndDia (
  ) where

import Lib
import HigherOrder
import NonDet
import End

-- box :: (Functor f, Functor f') => (f (Free (f + f') a) -> b) -> Free (f + f') a -> Free (f + f') [b]
-- box _ (Pure _) = Pure []
-- box f (Op g) = case g of
--                  L y ->  fmap (\x -> f y : x) (Op (fmap (box f) y))
--                  -- L y -> f g : box 

-- box :: (Functor f, Functor f') => (f [b] -> [b]) -> Free (f + f') a -> [b]
-- box g (Pure x) = []
-- box g (Op f) = fold (\a -> []) (case f of
--                                    L y -> 
--                                    R y -> )

-- fold :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
-- fold gen _   (Pure x) = gen x
-- fold gen alg (Op f)   = alg (fmap (fold gen alg) f)


-- box :: (Functor f, Functor f') => (f [b] -> [b]) -> Free (f + f') a -> [b]
-- box f (Pure x) = []
-- box g (Op f) = case f of
--                  L y -> g f

-- data Boxer f a f' b
--   = Boxer { 
--            bxr :: f [b] -> [b] }
--
-- box :: (Functor f, Functor f')
--        => Boxer f a f' b -> Free (f + f') a -> [b]
-- box h = fold
--   (\x -> [])
--   (\x -> case x of
--      L y -> bxr b y
--      R y -> Op y)

-- box :: Functor f => (f [b] -> [b]) -> Free f a -> [b]
-- box f = fold (\x -> []) (\x -> f x)
--
-- boxPuts :: (f [b] -> [b]) -> Free (f + End) a -> s -> [s]
-- boxPuts f (Pure _) p = []
-- boxPuts f (Op s) p = case s of
--                      L g -> f g : boxPuts g p
-- not right bcs gotta add a variable everytime. Impossible atm because value always wrapped in a context. 
-- gotta find a way to unrwap the context. I believe impossible without context of the action.

-- boxPuts :: Free (State s + End) a -> s -> [s]
-- boxPuts (Pure _) p = []
-- boxPuts (Op s) p = case s of
--                      L (Put s k) -> s : boxPuts k p
--                      L (Get k) -> boxPuts (k p) p


-- data Handler f a f' b
--   = Handler { ret  :: a -> Free f' b
--             , hdlr :: f (Free f' b) -> Free f' b }
--
-- handle :: (Functor f, Functor f')
--        => Handler f a f' b -> Free (f + f') a -> Free f' b
-- handle h = fold
--   (ret h)
--   (\x -> case x of
--      L y -> hdlr h y
--      R y -> Op y)

