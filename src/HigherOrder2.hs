
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module HigherOrder2 (
  ) where

import Lib
import Prelude hiding (sum)
import HigherOrder
import End

type f ->: g = forall a. f a -> g a

class (forall f. Functor (h f)) => HOFunctor h where
  hmap :: (f ->: g) -> (h f ->: h g)
  {- which is a natural transformation; i.e.,
       hmap g . fmap f = fmap f . hmap g -}

data Hefty h a
  = Return a
  | Do (h (Hefty h) (Hefty h a))

data Reader r m k
  = Ask (r -> k)
  | forall a. Local (r -> r) (m a) (a -> k)

deriving instance Functor (Reader r m)

instance HOFunctor (Reader r) where
  hmap _ (Ask k)       = Ask k
  hmap f (Local g m k) = Local g (f m) k
  
data A f (m :: * -> *) k = A (f k) deriving Functor

instance Functor f => HOFunctor (A f) where
  hmap _ (A x) = A x

infixr 6 :+
data (h1 :+ h2) (m :: * -> *) k = HL (h1 m k) | HR (h2 m k)
  deriving Functor

instance (HOFunctor h1, HOFunctor h2) => HOFunctor (h1 :+ h2) where
  hmap f (HL x) = HL (hmap f x)
  hmap f (HR x) = HR (hmap f x)



data Output x k = Out x k deriving Functor

out :: Output x <: f => x -> Free f ()
out x = Op (inj' (Out x (Pure ())))

hOut :: Functor f' => Handler (Output x) a f' (a, [x])
hOut = Handler
  { ret = \x -> pure (x, [])
  , hdlr = \x -> case x of
      Out y k -> fmap (\(v,ys) -> (v,y:ys)) k }

type h1 :-> h2 = forall (m :: * -> *) k. h1 m k -> h2 m k

data h1 <-:-> h2 = HOIso { hoto :: h1 :-> h2, hofrom :: h2 :-> h1 }
{- which satisifies
     hoto . hofrom = id
     hofrom . hoto = id -}

data HOForephism h1 h2
  = forall h3. (HOFunctor h1, HOFunctor h2, HOFunctor h3) =>
      HOForephism { hoiso :: h2 <-:-> (h1 :+ h3) }

infixr 4 :<
class (HOFunctor h1, HOFunctor h2) => h1 :< h2 where
  hoforephism :: HOForephism h1 h2

hoinj :: h1 :< h2 => h1 m a -> h2 m a
hoinj = case hoforephism of
  HOForephism i -> hofrom i . HL

instance HOFunctor h => h :< h

instance {-# OVERLAPPING #-} (HOFunctor h1, HOFunctor h2) => h1 :< h1 :+ h2

instance (HOFunctor h1, HOFunctor h2, HOFunctor h3, h1 :< h3)
      => h1 :< h2 :+ h3

ask :: Reader r :< h => Hefty h r
ask = Do (hoinj (Ask Return))

local :: Reader r :< h => (r -> r) -> Hefty h a -> Hefty h a
local f m = Do (hoinj (Local f m Return))



a :: (A f :< h, Functor f) => Free f a -> Hefty h a
a = fold Return (Do . hoinj . A)

-- localout0 :: Hefty (Reader Int :+ A (Output Int) :+ A End) ()
-- localout0 = local (+ (1 :: Int)) (do
--   (i :: Int) <- ask
--   a (out i)) -- Haskell does not infer that `f` is `Output Int` here

oa :: A (Output Int) :< h => Free (Output Int) a -> Hefty h a
oa = a

localout :: Hefty (Reader Int :+ A (Output Int) :+ A End) ()
localout = local (+ (1 :: Int)) (do
  (i :: Int) <- ask
  oa (out i))

data h :=> g
  = HA { ha :: forall a. h g (g a) -> g a }

hfold :: HOFunctor h
      => (forall a. a -> g a)
      -> h :=> g
      -> (Hefty h ->: g)
hfold gen _   (Return x) = gen x
hfold gen alg (Do x)     =
  ha alg (fmap (hfold gen alg) (hmap (hfold gen alg) x))

eA :: g <: f => A g :=> Free f
eA = HA (\(A x) -> Op (inj' x))

data AAsk r k = AAsk (r -> k) deriving Functor

aask :: AAsk r <: f => Free f r
aask = Op (inj' (AAsk Pure))

hAAsk :: Functor f' => r -> Handler (AAsk r) a f' a
hAAsk r = Handler
  { ret = pure
  , hdlr = \x -> case x of AAsk k -> k r }

eReader :: AAsk r <: f => Reader r :=> Free f
eReader = HA (\x -> case x of
  Ask k       -> aask >>= k
  Local f m k -> do
    r <- aask
    hup (handle (hAAsk (f r))) m >>= k)

infixr 6 /\
(/\) :: h1 :=> g -> h2 :=> g -> (h1 :+ h2) :=> g
a1 /\ a2 = HA (\x -> case x of
  HL x -> ha a1 x
  HR y -> ha a2 y)

run :: Hefty (Reader Int :+ A (Output Int) :+ A End) a -> (a, [Int])
run x = let
    y :: Free (AAsk Int + Output Int + End) _
      = hfold Pure (eReader /\ eA /\ eA) x   -- first elaborate
  in un (handle hOut (handle (hAAsk 41) y))  -- then handle

data State s k = Put s k | Get (s -> k) deriving Functor

get :: State s <: f => Free f s
get = Op (inj' (Get Pure))

put :: State s <: f => s -> Free f ()
put s = Op (inj' (Put s (Pure ())))

hState :: Functor g => Handler_ (State s) a s g (a, s)
hState = Handler_
  { ret_ = \x s -> pure (x, s)
  , hdlr_ = \x s -> case x of
      Put s' k -> k s'
      Get k -> k s s }

eReader' :: State Int <: f => Reader Int :=> Free f
eReader' = HA (\x -> case x of
  Ask k       -> get >>= k
  Local f m k -> do
    (i :: Int) <- get
    put (f i)
    v <- m
    put i
    k v)

run' :: Hefty (Reader Int :+ A (Output Int) :+ A End) a -> ((a, Int), [Int])
run' x = let
    y :: Free (State Int + Output Int + End) _
      = hfold Pure (eReader' /\ eA /\ eA) x  -- first elaborate
  in un (handle hOut (handle_ hState y 41))  -- then handle

ffold :: forall h g a b.
         HOFunctor h          -- as hfold
      => (forall c. c -> g c) -- as hfold
      -> h :=> g              -- as hfold
      -> (a -> b)             -- base generator
      -> (h g b -> b)         -- base algebra
      -> Hefty h a
      -> b
ffold _   _ genb _  (Return x) = genb x
ffold gen a genb ab (Do x) =
  ab                                    -- 3.
     (hmap (hfold gen a)                -- 2.
       (fmap (ffold gen a genb ab) x))  -- 1.



instance HOFunctor h => Monad (Hefty h) where
  m >>= k = ffold Return (HA Do) k Do m

instance HOFunctor h => Applicative (Hefty h) where
  pure = Return
  f <*> m = ffold Return (HA Do) (flip fmap m) Do f

instance HOFunctor h => Functor (Hefty h) where
  fmap f = ffold Return (HA Do) (pure . f) Do
