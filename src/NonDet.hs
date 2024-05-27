
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE LambdaCase #-}

module NonDet (
  FreeND(..),
  HandlerND(..),
  HandlerToND(..),
  handleToND,
  handleND,
  ) where

import Lib 

--obsolete, all of it, but kept because took a long time, and also because it seems like an implementation that would more
-- easily support parallelism than the new one in Choose.hs

data FreeND f a = PureND a | OpND (f [FreeND f a]) -- maybe later will have to make monad, functor and applicative instances

-- data Root f a = Root [FreeND f a]

-- foldRoot :: Functor f => (a -> [FreeND f b]) -> (f [FreeND f b] -> [FreeND f b]) -> Root f a -> Root f b
-- foldRoot gen alg (Root xs) = Root (foldND gen alg xs)

data HandlerToND f a f' b = HandlerToND { retToND :: a -> [FreeND f' b], hdlrToND :: f [FreeND f' b] -> [FreeND f' b]}

handleToND :: (Functor f, Functor f') => HandlerToND f a f' b -> Free (f + f') a -> [FreeND f' b]
handleToND h = fold
    (retToND h)
    (\case 
         L y -> hdlrToND h y
         R y -> [OpND y])


foldND :: Functor f => (a -> [b]) -> (f [b] -> [b]) -> [FreeND f a] -> [b]
foldND _ _   [] = []
foldND gen alg   (PureND x : xs) = gen x ++ foldND gen alg xs
foldND gen alg (OpND f : xs)   = alg (fmap (foldND gen alg) f) ++ foldND gen alg xs


data HandlerND f a f' b = HandlerND { retND :: a -> [FreeND f' b], hdlrND :: f [FreeND f' b]-> [FreeND f' b]}

-- unable to use any kind of fold function here
handleND :: (Functor f, Functor f') => HandlerND f a f' b -> [FreeND (f + f') a] -> [FreeND f' b]
handleND _ [] = []
handleND h (PureND x : xs) = retND h x ++ handleND h xs
handleND h (OpND f : xs) = case f of
                                 L y -> hdlrND h (fmap (handleND h) y) ++ handleND h xs
                                 R y -> OpND (fmap (handleND h) y) : handleND h xs

-- data HandlerND_ f a p f' b = HandlerND_ {retND_ :: a -> [p -> FreeND f' b], hdlrND_ :: f [p -> FreeND f' b] -> p -> [FreeND f' b]}
-- -- latest attempt here
-- handleND_ :: (Functor f, Functor f') => HandlerND_ f a p f' b -> [FreeND (f + f') a] -> p -> [FreeND f' b]
-- handleND_ _ [] _ = []
-- handleND_ h (PureND x : xs) p =  map (\g -> g p) (retND_ h x) ++ handleND_ h xs p
-- handleND_ h (OpND f : xs) p = case f of
--                               L x -> hdlrND_ h (fmap (flip (handleND_ h) p) x) p ++ handleND_ h xs p
--                               R y -> OpND (fmap (flip (handleND_ h) p) y) : handleND_ h xs p


-- data HandlerND_ f a p f' b = HandlerND_ {retND_ :: a -> p -> [FreeND f' b], hdlrND_ :: f [FreeND f' b] -> p -> [FreeND f' b]}
-- -- latest attempt here
-- handleND_ :: (Functor f, Functor f') => HandlerND_ f a p f' b -> [FreeND (f + f') a] -> p -> [FreeND f' b]
-- handleND_ _ [] _ = []
-- handleND_ h (PureND x : xs) p =  retND_ h x p ++ handleND_ h xs p
-- handleND_ h (OpND f : xs) p = case f of
--                               L x -> hdlrND_ h (fmap (flip (handleND_ h) p) x ) p ++ handleND_ h xs p
--                               R y -> OpND (fmap (flip (handleND_ h) p) y) : handleND_ h xs p

-- hChooseToND :: Functor f' => HandlerToND Choose a f' a
-- hChooseToND = HandlerToND
--   { retToND = \x -> [PureND x]
--   , hdlrToND = \x -> case x of Choose f -> f True ++ f False}
