
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Conc (
  par,
  ) where

import Lib
import HigherOrder
import Choose
import Data.Tuple (swap)


par :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- par (Pure x) y = fmap (x,) y
-- par x (Pure y) = fmap (,y) x
par x y = do
  goesFirst x y ~+~ fmap swap (goesFirst y x)

goesFirst :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
goesFirst (Pure x) y = fmap (x,) y
goesFirst (Op x) y = Op (fmap (`par` y) x)

lpar :: Choose <: f => [Free f a] -> Free f [a]
lpar = undefined

lgoesFirst :: Choose <: f => Int -> [Free f a] -> Free f [a]
lgoesFirst n xs = undefined

-- goesSecond :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- goesSecond x (Pure y) = fmap (,y) x
-- goesSecond x (Op f) = Op (fmap (par x) f)

-- rule M: m1 || m2 == (m1 |L m2) + (m2 |L m1)
-- rule LM1 (ish; the rule is a little different): pure x |L m == fmap (\y -> (x,y)) m
-- rule LM2: zero |L m == zero
-- rule LM3: op (fmap k x) |L m == op (fmap (\x -> (k x) || m) x)
-- m1 || m2 == fmap swap (m2 || m1)
-- m1 || (m2 || m3) == fmap assoc ((m1 || m2) || m3)

-- m1 || m2 == par m1 m2
-- m1 |L m2 == goesFirst m1 m2 or goesSecond m2 m1 depending on the context 
-- (depends on whether m1 or m2 is supposed to end up as the first result in the tuple)
-- '+' is denoting the nondeterministic branching into different possible results. So it is equivalent to ~+~

-- so rule M === par m1 m2 == goesFirst m1 m2 ~+~ goesSecond m1 m2, which is literally its definition (1 step unfolding)

-- rule LM1 === goesFirst (pure x) m = fmap (x,) m, which is also just a one step unfolding

-- rule LM2 === goesFirst zero m == zero
--              goesFirst (Op Zero) m == zero
--              Op (fmap (`par` m) Zero) == zero
--              Op Zero == zero

-- rule LM3 === goesFirst (Op (fmap k x)) m == Op (fmap (\x -> par (k x) m) x)
--              Op (fmap (`par` m) (fmap k x)) == Op (fmap (\x -> par (k x) m) x)
--              Op (fmap (\x -> par x m) (fmap k x)) == Op (fmap (\x -> par (k x) m) x)
--              Op (fmap ((\x -> par x m) . k) x) == Op (fmap (\x -> par (k x) m) x)
--              Op (fmap (\x -> par (k x) m) x) == Op (fmap (\x -> par (k x) m) x)
--
-- commutativity === par m1 m2 == fmap swap (par m2 m1)
                  -- par m1 m2 == fmap swap (goesFirst m2 m1 ~+~ goesSecond m2 m1)
                  -- goesFirst m1 m2 ~+~ goesSecond m1 m2 == fmap swap (goesFirst m2 m1 ~+~ goesSecond m2 m1)
                  -- (Op (Choose (\k -> if k then (goesFirst m1 m2) else (goesSecond m1 m2)))) == fmap swap (Op (Choose (\k -> if k then (goesFirst m2 m1) else (goesSecond m2 m1))))
--
--NEW First prove lemma:
--a: fmap f (Op (Choose (\k -> if k then a else b))) == Op (Choose (\k -> if k then fmap f a else fmap f b))
--
--b: fmap (par (Pure x)) f == fmap (x,) f
--   
--c: fmap (`par` (Pure x)) f == fmap (,x) f
--
--d: don't know if we can prove this, but somehow justify that x ~+~ y == y ~+~ x
--
--e: par (Pure x) (Pure y) == Pure (x,y)
--
--f: par (par (Pure x) (Pure y)) (Pure z) == Pure ((x,y),z)
--
--g: par (Pure x) (par (Pure y) (Pure z)) == Pure (x,(y,z))
--
--

--
--base case: m1 = Pure x and m2 = Pure y
--
-- (Op (Choose (\k -> if k then (goesFirst (Pure x) (Pure y)) else (goesSecond (Pure x) (Pure y))))) == fmap swap (Op (Choose (\k -> if k then (goesFirst (Pure y) (Pure x)) else (goesSecond (Pure y) (Pure x)))))
--
-- -- Expand the goesFirst's and goesSecond's
-- (Op (Choose (\k -> if k then (fmap (x,) (Pure y)) else (fmap (,y) (Pure x))))) == fmap swap (Op (Choose (\k -> if k then (fmap (y,) (Pure x)) else (fmap (,x) (Pure y)))))
-- (Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x, y))))) == fmap swap (Op (Choose (\k -> if k then (Pure (y,x)) else (Pure (y,x)))))
--
-- -- Here, use the functor instance for Free f a to fold the swap
-- (Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x,y))))) == fold (pure . swap) Op (Op (Choose (\k -> if k then (Pure (y,x)) else (Pure (y,x)))))
-- (Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x,y))))) == Op (fmap (fold (pure . swap) Op) (Choose (\k -> if k then (Pure (y,x)) else (Pure (y,x)))))
--
-- -- Then, simply use the auto-derived instances for functor for Choose
-- (Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x,y))))) == Op (Choose (\k -> if k then (fold (pure . swap) Op (Pure (y,x))) else (fold (pure . swap) Op (Pure (y,x)))))
-- (Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x,y))))) == Op (Choose (\k -> if k then (pure . swap (y,x)) else (pure . swap (y,x))))
-- (Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x,y))))) == Op (Choose (\k -> if k then (Pure (x,y)) else (Pure (x,y))))
-- which proves our base case
-- 
-- intermediary case: m1 = Pure x, m2 = Op g
--For this one, we are actually gonna need some kind of handling to show that the case for k = True and k = False are equivalent/interchangeable.
--
-- (Op (Choose (\k -> if k then (goesFirst (Pure x) (Op g)) else (goesSecond (Pure x) (Op g))))) == fmap swap (Op (Choose (\k -> if k then (goesFirst (Op g) (Pure x)) else (goesSecond (Op g) (Pure x)))))
-- (Op (Choose (\k -> if k then (fmap (x,) (Op g)) else (Op (fmap (par (Pure x)) g))))) == fmap swap (Op (Choose (\k -> if k then (Op (fmap (`par` (Pure x)) g)) else (fmap (,x) (Op g)))))
--
-- -- Here we will go ahead and skip the step of unfolding fold etc to end up with swap in the right place. This was done in detail in the base case
-- Op (Choose (\k -> if k then (fmap (x,) (Op g)) else (Op (fmap (par (Pure x)) g)))) == Op (Choose (\k -> if k then (fold (pure . swap) Op (Op (fmap (`par` (Pure x)) g))) else (fold (pure . swap) Op (fmap (,x) (Op g)))))
-- Op (Choose (\k -> if k then (fmap (x,) (Op g)) else (Op (fmap (par (Pure x)) g)))) == Op (Choose (\k -> if k then (Op (fmap (fold (pure . swap) Op) (fmap (`par` (Pure x)) g))) else (fmap swap (fmap (,x) (Op g)))))
-- Op (Choose (\k -> if k then (fmap (x,) (Op g)) else (Op (fmap (par (Pure x)) g)))) == Op (Choose (\k -> if k then (Op (fmap ((fold (pure . swap) Op) . (`par` (Pure x))) g)) else (fmap (swap . (,x)) (Op g))))
-- Op (Choose (\k -> if k then (fmap (x,) (Op g)) else (Op (fmap (par (Pure x)) g)))) == Op (Choose (\k -> if k then (Op (fmap ((fmap swap) . (`par` (Pure x))) g)) else (fmap (x,) (Op g))))
-- 
-- -- For this we have to assume that x ~+~ y == y ~+~ x, which is followed by, in the extension of x ~+~ y as Op (Choose (\k -> if k then x else y)), the 'then' and the 'else' statement can be freely inverted.
-- -- The first part of the first and the second of the second are already equal. The others result in the following inequality:
-- fmap (par (Pure x)) g == fmap ((fmap swap) . (`par` (Pure x))) g
-- -- which looks a lot like the initial inequation, so we would like to somehow define the recursive step here but I don't know how
-- since g is supposed to be an effect of the form g = f g', fmap (par (Pure x)) g == f (par (Pure x) g'), and fmap ((fmap swap) . (`par` (Pure x))) g == f (fmap swap (par g' (Pure x))) so:
-- par (Pure x) g' == fmap swap (par g' (Pure x))
-- -- which can be used for the recursive call.
-- -- we can follow the same procedure for m1 = Op f and m2 = Pure y, and it would yield as the recursive call:
-- par f' (Pure y) = fmap swap (par (Pure x) f')
-- -- Finally in the last recursive call, the most generalist one with m1 = Op f and m2 = Op g, we would end up with 2 recursive calls, which are:
-- par f g' == fmap swap (par g' f)
-- -- and
-- par f' g == fmap swap (par g f')
-- -- All of those work as recursive calls, because f' and g' are continuation of an Op, and should eventually result in a Pure, which would then eventually result in the base case.
-- -- Here we are facing a two dimensional proof by induction, which works, as each step going down from f to f' or from g to g' brings the Free f a closer to its pure. 
-- -- Once one of the pure is hit, then we end up in a lower recursive step, which will work to bring the other Free monad down to a pure, which will then results in the actual base case.
--
--
-- m1 || (m2 || m3) == fmap assoc ((m1 || m2) || m3)
-- par m1 (par m2 m3) == fmap assoc (par (par m1 m2) m3)
-- goesFirst m1 (par m2 m3) ~+~ goesSecond m1 (par m2 m3) == fmap assoc (goesFirst m1 (par m2 m3) ~+~ goesSecond m1 (par m2 m3))
-- Op (Choose (\k -> if k then goesFirst m1 (par m2 m3) else goesSecond m1 (par m2 m3))) == fmap assoc (Op (Choose (\k -> if k then goesFirst (par m1 m2) m3 else goesSecond (par m1 m2) m3)))
--
-- base case, m1 = Pure x, m2 = Pure y, m3 = Pure z
-- Op (Choose (\k -> if k then goesFirst (Pure x) (par (Pure y) (Pure z)) else goesSecond (Pure x) (par (Pure y) (Pure z)))) == fmap assoc (Op (Choose (\k -> if k then (goesFirst (par (Pure x) (Pure y)) Pure z) else (goesSecond (par (Pure x) (Pure y)) (Pure z)))))
--
-- Here i take a shortcut and assume that par (Pure a) (Pure b) == Pure (a,b)
-- Op (Choose (\k -> if k then (fmap (x,) (Pure (y,z))) else (goesSecond (Pure x) (Pure (y,x))))) == fmap assoc (Op (Choose (\k -> if k then (goesFirst (Pure (x,y)) (Pure z)) else (fmap (,z) (Pure (x,y))))))
-- Op (Choose (\k -> if k then (Pure (x,(y,z))) else (fmap (,(y,z) (Pure x))))) == fmap assoc (Op (Choose (\k -> if k then (fmap ((x,y),) (Pure z)) else (Pure ((x,y),z)))))
-- Op (Choose (\k -> if k then (Pure (x,(y,z))) else (Pure (x, (y,z))))) == fmap assoc (Op (Choose (\k -> if k then (Pure ((x,y),z)) else (Pure ((x,y),z)))))
--
-- Op (Choose (\k -> if k then (Pure (x,(y,z))) else (Pure (x, (y,z))))) == fold (pure . assoc) Op (Op (Choose (\k -> if k then (Pure ((x,y),z)) else (Pure ((x,y),z)))))
-- Op (Choose (\k -> if k then (Pure (x,(y,z))) else (Pure (x, (y,z))))) == (Op (Choose (\k -> if k then (fold (pure. assoc) Op (Pure ((x,y),z))) else (fold (pure. assoc) Op (Pure ((x,y),z))))))
-- Op (Choose (\k -> if k then (Pure (x,(y,z))) else (Pure (x, (y,z))))) == (Op (Choose (\k -> if k then (pure . assoc ((x,y),z)) else (pure . assoc ((x,y), z)))))
-- Op (Choose (\k -> if k then (Pure (x,(y,z))) else (Pure (x, (y,z))))) == (Op (Choose (\k -> if k then (Pure (x,(y,z))) else (Pure (x,(y,z))))))
-- which concludes our base case.
--
-- m1 = Op f, m2 = Pure y, m3 = Pure z
-- Op (Choose (\k -> if k then goesFirst (Op f) (par (Pure y) (Pure z)) else goesSecond (Op f) (par (Pure y) (Pure z)))) == fmap assoc (Op (Choose (\k -> if k then (goesFirst (par (Op f) (Pure y)) Pure z) else (goesSecond (par (Op f) (Pure y)) (Pure z)))))
-- Op (Choose (\k -> if k then goesFirst (Op f) (Pure (y,z)) else goesSecond (Op f) (Pure (y,z)))) == fmap assoc (Op (Choose (\k -> if k then (goesFirst (par (Op f) (Pure y)) Pure z) else (goesSecond (par (Op f) (Pure y)) (Pure z)))))
--
--par (Op f) (Pure y) == goesFirst (Op f) (Pure y) ~+~ goesSecond (Op f) (Pure y)
--                    == Op (Choose (\k -> if k then (Op (fmap (`par` (Pure y)) f)) else (fmap (,y) (Op f))))
--
-- from this we derive:
-- goesFirst (par (Op f) (Pure z)) Pure z == Op (fmap (`par` (Pure z)) (Choose (\k -> if k then (Op (fmap (`par` (Pure y)) f)) else (fmap (,y) f))))
--                                        == Op (Choose (\k -> if k then (Op (fmap ((`par` (Pure z)) . (`par` (Pure y))) f)) else fmap ((,z) . (,y)) (f)))
--                                        == Op (Choose (\k -> if k then (Op fmap (x -> (x,y),z)) else ()))
--
-- Okay I think the plan is to have assumptions at the start, such as fmap (`par` (Pure a)) == fmap (,a) and fmap (par (Pure a)) = fmap (a,), on tope of par (Pure a) (Pure b) = Pure (a,b), this way those base case would go infinitely faster.
--
--
-- par :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- par program1 program2 = do
--   goesFirst program1 program2 ~+~ goesSecond program1 program2
--
-- goesFirst :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- goesFirst (Pure x) y = fmap (x,) y
-- goesFirst (Op f) y = Op (fmap (`par` y) f)
--
-- goesSecond :: Choose <: f => Free f a -> Free f b -> Free f (a, b)
-- goesSecond x (Pure y) = fmap (,y) x
-- goesSecond x (Op f) = Op (fmap (par x) f)
--
--
--
--
--
--
--
--
--
--
--
--

