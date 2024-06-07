{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ABPModel (
  -- abpmodel,
  dabpmodel,
  Status(..),
  ) where

import Lib
import HigherOrder
import Choose
import Conc
import State
import LockConc

-- For the state sent by the sending program, where Switch represents the switching bool and empty the end of the sending
-- data Status = Switch Bool | Empty 
--
-- sendingProgram :: (State Bool <: f, State Status <: f, State a <: f) => Bool -> [a] -> Free f ()
-- sendingProgram _ [] = do 
--   put' Empty
--   Pure ()
-- sendingProgram b (x:xs) = do
--   (cond::Bool) <- get'
--   if cond == b then sendingProgram (not b) xs else do
--     sendingProgram b (x:xs)
--
-- receivingProgram :: (State Bool <: f, State Status <: f, State a <: f) => Bool -> Free f [a] 
-- receivingProgram b = do
--   (cond::Status) <- get'
--   case cond of
--     Switch cond' -> if cond' /= b then do (something::a) <- get'; fmap ([something] ++ ) (receivingProgram (not b)) 
--                   else do
--                     receivingProgram b
--     Empty -> do
--       (something::a) <- get'
--       Pure [something]
--
-- abpmodel :: (Choose <: f, State Bool <: f, State Status <: f, State a <: f) => [a] -> Free f ((),[a])
-- abpmodel xs = par (sendingProgram True xs) (receivingProgram True)
 
-- dabpmodel :: Choose <: f => Int -> [a] -> Free f ((),[a])
-- dabpmodel n (x:xs) = handle_ hStateS (handle_ hStateS (handle_ hStateS (dPar n (sendingProgram False xs) (receivingProgram True)) (Switch False)) True) x

data Status = Switch Bool | Empty 

sendingProgram :: (State Bool <: f, State Status <: f, State a <: f, Lock <: f) => Bool -> [a] -> Free f ()
sendingProgram _ [] = do 
  put' Empty
  Pure ()
sendingProgram b (x:xs) = do
  unlock;lock
  (cond::Bool) <- get'
  if cond == b then do put' x;put' (Switch (not cond));sendingProgram (not b) xs else sendingProgram b (x:xs)

receivingProgram :: (Lock <: f, State Bool <: f, State Status <: f, State a <: f) => Bool -> Free f [a] 
receivingProgram b = do
  unlock;lock
  (cond::Status) <- get'
  case cond of
    Switch cond' -> if cond' /= b then do (something::a) <- get';put' cond';fmap (something:) (receivingProgram (not b)) else receivingProgram b
    Empty -> do (something::a) <- get'; return [something]

dabpmodel :: Choose <: f => Int -> [a] -> Free f ((),[a])
dabpmodel n (x:xs) = handle_ hStateS 
                      (handle_ hStateS 
                        (handle_ hStateS 
                          (handle hLock 
                            (nlockPar n (sendingProgram False xs) (receivingProgram True))) 
                        (Switch False)) 
                      True) 
                    x


