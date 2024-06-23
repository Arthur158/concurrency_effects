{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}


-- different programs using our interface and effects
module Programs (
  lockProgram,
  lockProgram2,
  parWithLock,
  choice,
  choiceplus,
  choicealt,
  choiceAndError,
  example1,
  example2,
  program1,
  program2,
  program3,
  pairing,
  triplepairing,
  triplepairing2
) where

import Lib
import State
import HigherOrder
import Err
import Choose
import Control.Applicative
import Conc
import LockConc
import NewConc

choice :: Choose <: f => Free f Int
choice = do
  b <- choose
  if b then pure 0 else pure 1

choiceplus :: Choose <: f => Free f Int
choiceplus = pure 0 ~+~ pure 1 ~+~ pure 2

choicealt :: Choose <: f => Free f Int
choicealt = pure 0 <|> pure 1 <|> pure 2

choiceAndError :: (Err <: f, Choose <: f) => Free f Int
choiceAndError = do
  b <- choose
  if b then pure 0 else err' "fails here"
  c <- choose
  if c then pure 1 else err' "fails there"

example1 :: Free (State Int) Int
example1 = Op (Get (\s -> Op (Put (s + 1) (Pure s))))

example2 :: Free (State Int) Int
example2 = do s <- get'; put (s+1); return s

program1 :: (Choose <: f,State Int <: f, Err <: f) => Free f Int
program1 = do
  (s::Int) <- get'
  put' (s + 4)
  (s::Int) <- get'
  if s > 7 then err' "foo" else Pure s

program2 :: State Int <: f => Free f Bool
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

pairing :: (Choose <: f, State Int <: f, Err <: f) => Free f (Int, Bool)
pairing = par program1 program2

triplepairing :: (Choose <: f, State Int <: f, Err <: f) => Free f ((Int, Bool), Bool)
triplepairing = par (par program1 program2) program3

triplepairing2 :: (Choose <: f, State Int <: f, Err <: f) => Free f ((Int, Bool), Bool)
triplepairing2 = newpar (newpar program1 program2) program3

lockProgram :: (Lock <: f, State Int <: f) => Free f Int
lockProgram = do
  lock
  (s :: Int) <- get'
  put' (s + 1)
  unlock
  get'

lockProgram2 :: (Lock <: f, State Int <: f) => Free f Int
lockProgram2 = do
  lock
  (s :: Int) <- get'
  put' (4::Int)
  unlock
  put' (s + 1)
  get'

parWithLock :: (State Int <: f, Choose <: f) => Free (Lock + f) (Int, Int)
parWithLock = lockPar lockProgram lockProgram2
