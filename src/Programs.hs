{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Programs (
  incerr
  , incerr'
  , progr1
  , progr2
  , par6
  , useZero
  , preinc
  , ancerr'
  , stateb
  , pureChooseProg
  , ancerrDouble
  , par1
  , par3
  , makeState
  , twitch
  , newpar
  -- , parWithLock
  -- , parWithLock2
  -- , parWithoutLock
  -- , parWithLock3
  , tst
  , choice
  , choice_and_error
  -- , something
  , parnew
  , pairing
  , triplepairing
  , triplepairing2
  , pickNumber
) where

import Lib
import State
import HigherOrder
import End
import Err
import Choose
import Control.Applicative
import Conc
import LockConc

state :: (Functor f, State Int <: f, End <: f, Lock <: f) => Int -> Free f Int
state a = do
  put' a
  s <- get'
  pure s

state' :: (Functor f, State Int <: f, Choose <: f, End <: f, Lock <: f) => Int -> Free f Int
state' a = do
  s <- get'
  pure s


tst :: (Functor f, State Int <: f, Choose <: f, End <: f) => Free (Lock + f) (Int, Int)
tst = lockPar (state 1) (state' 1)

lockProgram :: (Functor f, Lock <: f, End <: f, State Int <: f,Err <: f) => Free f Int
lockProgram = do
  lock
  (s :: Int) <- get'
  put' (s + 1)
  unlock
  get'

lockProgram2 :: (Functor f, Lock <: f, State Int <: f,Err <: f) => Free f Int
lockProgram2 = do
  lock
  (s :: Int) <- get'
  put' (4::Int)
  unlock
  put' (s + 1)
  get'

lockProgram3 :: (Functor f, Lock <: f, State Int <: f,Err <: f) => Free f Int
lockProgram3 = do
  lock
  (s::Int) <- get' 
  put' (4::Int)
  unlock
  (s :: Int) <- get'
  put' (s + 1)
  get'

lockProgram4 :: (Functor f, Lock <: f, State Int <: f,Err <: f) => Free f Int
lockProgram4 = do
  (s::Int) <- get' 
  put' (s + 3)
  lock
  put' (4::Int)
  (s :: Int) <- get'
  put' (s + 2)
  unlock
  get'

lockProgram5 :: (Functor f, State Int <: f,Err <: f) => Free f Int
lockProgram5 = do
  (s::Int) <- get' 
  put' (4::Int)
  (s :: Int) <- get'
  put' (s + 1)
  get'

-- parWithLock :: (Functor f, End <: f, State Int <: f, Err <: f, Choose <: f) => Free (Lock + f) (Int, Int)
-- parWithLock = par lockProgram lockProgram2
--
-- parWithLock2 :: (Functor f, End <: f, State Int <: f, Err <: f, Choose <: f) => Free (Lock + f) ((Int, Int), Int)
-- parWithLock2 = lockPar (lockPar lockProgram lockProgram2) lockProgram
--
-- parWithLock3 :: (Functor f, End <: f, State Int <: f, Err <: f, Choose <: f) => Free (Lock + f) (Int, Int)
-- parWithLock3 = lockPar lockProgram4 lockProgram4 
--
-- something :: (Functor f, End <: f,State Int <: f, Err <: f, Choose <: f) => Free f (Int, Int)
-- something = par lockProgram5 lockProgram5 
--
-- parWithoutLock :: (Functor f, End <: f, State Int <: f, Err <: f, Choose <: f, Lock <: f) => Free f (Int, Int)
-- parWithoutLock = par lockProgram lockProgram2

incerr :: Free (Err + State Int + End) a
incerr =  Op (R (L (Get (\s ->
            Op (R (L (Put (s + 1)
              (Op (L (Err "foo"))))))))))

-- incerr' :: Free (Err + State Int + End) a
-- incerr' = do
--   (s :: Int) <- get
--   put (s + 1)
--   err "foo"

incerr' :: (Functor f, End <: f, State Int <: f,Err <: f) => Free f a
incerr' = do
  (s :: Int) <- get'
  put' (s + 1)
  err' "foo"

incerr'2 :: (Functor f, End <: f, State Int <: f,Err <: f) => Free f a
incerr'2 = do
  (s :: Int) <- get'
  put' (s + 2)
  err' "cuu"

choice :: Choose <: f => Free f Int
choice = do
  b <- choose
  if b then pure 0 else pure 1

choice_and_error :: (Err <: f, Choose <: f) => Free f Int
choice_and_error = do
  b <- choose
  if b then pure 0 else err' "fails here"
  c <- choose
  if c then pure 1 else err' "fails there"

ancerr' :: (Functor f, End <: f, State Int <: f,Err <: f, Choose <: f) => Free f a
ancerr' = do
  (s :: Int) <- get'
  (b :: Bool) <- choose
  if b
     then put' (s + 2)
     else put' (s + 1)
  err' "foo"

ytr :: (End <: f, State Int <: f, Choose <: f) => Free f Int
ytr = do
  (s :: Int) <- get'
  put' (s + 2)
  (j :: Int) <- get'
  if j > 3
     then zero
     else return 3
  return 2

xtr :: (Functor f, Err <: f) => Free f Int
xtr = do
  pure 4

btr :: (Functor f, State Int <: f) => Free f Bool
btr = do
  (s::Int) <- get'
  return True



parnew :: (Err <: f, End <: f, Choose <: f, State Int <: f) => Free (Choose + f) (Int, Bool)
parnew = par xtr btr


ancerrDouble :: (End <: f, State Int <: f,Err <: f, Choose <: f) => Free f a
ancerrDouble = do
  (b :: Bool) <- choose
  (s :: Int) <- get'
  (b2 :: Bool) <- choose
  if b
     then put' (s + 2)
     else put' (s + 1)
  if b2
     then err' "coo"
     else put' (s + 4)
  if b2 && b
     then err' "shoo"
     else put' (99 :: Int)
  (b3 :: Bool) <- choose
  _ <- if b3
     then err' "meuh"
     else put' (99 :: Int)
  (b4 :: Bool) <- choose
  if b4 && b3
     then err' "ceuu"
     else err' "shnoops"
  err' "foo"


useplus :: (End <: f, State Int <: f,Err <: f, Choose <: f) => Free f a
useplus = do
  (s :: Int) <- get'
  let p1 = put' (s + 2)
  let p2 = put' (s + 1)
  p1 <|> p2
  err' "foo"

twitch :: (Choose <: f, Err <: f) => Free f Bool
twitch = do
  err' "foo" ~+~ pure True

useZero :: Choose <: f => Free f Int
useZero = do
  zero ~+~ pure 3

useZero2 :: Choose <: f => Free f Int 
useZero2 = do
  pure 1 ~+~ zero

par6 :: Choose <: f => Free (Choose + f) (Int, Int)
par6 = par useZero2 useZero

progr1 :: (Choose <: f, Err <: f) => Free f Int
progr1 = do
  a <- choose
  (s::Int) <- pure 0
  s <- if a then pure (s+1) else pure (s+2)
  b <- choose
  s <- if b then pure (s+3) else pure (s+5)
  c <- choose
  if c then pure (s+7) else pure (s+11)

progr2 :: (Choose <: f, Err <: f) => Free (Choose + f) ((Int, Int), (Int, Int))
progr2 = par (par (pure 4) (pure 1)) (par (pure 3) (pure 2))


makeState :: (End <: f, State Int <: f,Err <: f) => Free f Int
makeState = do
  (s :: Int) <- get'
  return 4
  err' "foo"
  put' (s + 2)
  put' (s + 3)
  put' (s + 4)
  put' (s + 6)
  put' (s + 5)
  put' (s + 7)
  put' (s + 8)
  pure 4


par1 :: (End <: f, State Int <: f,Err <: f, Choose <: f) => Free f (a,a)
par1 = fpar ancerrDouble useplus

par3 :: (End <: f, State Bool <: f, State Int <: f,Err <: f, Choose <: f) => Free (Choose + f) (a,a)
par3 = par ancerrDouble stateb

stateb :: (End <: f, State Bool <: f,Err <: f, Choose <: f) => Free f a
stateb = do
  (b :: Bool) <- choose
  put' b
  err' "foo"

pureChooseProg :: (Err <: f, Choose <: f) => Free f a
pureChooseProg = do
  _ <- choose
  err' "foo"

preinc :: Free (State Int) Int
preinc = Op (Get (\s -> Op (Put (s + 1) (Pure s))))

newpar :: Choose <: f => Free (Choose + f) (Int, Int)
newpar = par (pure 4) (zero)


-- program1 :: (Choose <: f,State Int <: f, Lock <: f, Err <: f) => Free f Int
-- program1 = do
--   lock
--   (s::Int) <- get'
--   put' (s + 4)
--   unlock
--   (s::Int) <- get'
--   if s > 7 then err' "foo" else Pure s
--
-- program2 :: (State Int <: f, Lock <: f) => Free f Bool
-- program2 = do
--   lock
--   (s::Int) <- get'
--   put' (s+5)
--   unlock
--   (s::Int) <- get'
--   Pure (s > 7)
--
-- program3 :: (State Int <: f, Lock<: f) => Free f Bool
-- program3 = do
--   lock
--   (s::Int) <- get'
--   put' (s - 3)
--   unlock
--   (s::Int) <- get'
--   Pure (s < 4)
--
-- pairing :: (Choose <: f, State Int <: f, Err <: f) => Free (Lock + f) (Int, Bool)
-- pairing = lockPar program1 program2
--
-- triplepairing :: (Choose <: f, State Int <: f, Err <: f) => Free (Lock + f) ((Int, Bool), Bool)
-- triplepairing = lockPar (lockPar program1 program2) program3
--
-- triplepairing2 :: (Choose <: f, State Int <: f, Err <: f) => Free (Lock + f) (Int, (Bool, Bool))
-- triplepairing2 = lockPar program1 (lockPar program2 program3)




program1 :: (Choose <: f,State Int <: f, Err <: f) => Free f Int
program1 = do
  (s::Int) <- get'
  put' (s + 4)
  (s::Int) <- get'
  if s > 7 then err' "foo" else Pure s

program2 :: (State Int <: f) => Free f Bool
program2 = do
  (s::Int) <- get'
  put' (s+5)
  (s::Int) <- get'
  Pure (s > 7)

program3 :: (State Int <: f) => Free f Bool
program3 = do
  (s::Int) <- get'
  put' (s - 3)
  (s::Int) <- get'
  Pure (s < 4)

pairing :: (Choose <: f, State Int <: f, Err <: f) => Free (Choose + f) (Int, Bool)
pairing = par program1 program2


triplepairing :: (Choose <: f, State Int <: f, Err <: f) => Free f ((Int, Bool), Bool)
triplepairing = fpar (fpar program1 program2) program3

triplepairing2 :: (Choose <: f, State Int <: f, Err <: f) => Free (Choose + f) (Int, (Bool, Bool))
triplepairing2 = par program1 (par program2 program3)




pickNumber :: (Choose <: f) => Free f Int
pickNumber = do
  a <- pick 3
  b <- pick 4
  Pure (a*b)


