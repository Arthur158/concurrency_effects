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

-- data Status = Switch Bool | Empty 
--
-- sendingProgram :: (State Bool <: f, State Status <: f, State a <: f, Lock <: f) => Bool -> [a] -> Free f ()
-- sendingProgram _ [] = do 
--   put' Empty
--   Pure ()
-- sendingProgram b (x:xs) = do
--   unlock;lock
--   (cond::Bool) <- get'
--   if cond == b then do put' x;put' (Switch (not cond));sendingProgram (not b) xs else sendingProgram b (x:xs)
--
-- receivingProgram :: (Lock <: f, State Bool <: f, State Status <: f, State a <: f) => Bool -> Free f [a] 
-- receivingProgram b = do
--   unlock;lock
--   (cond::Status) <- get'
--   case cond of
--     Switch cond' -> if cond' /= b then do (something::a) <- get';put' cond';fmap (something:) (receivingProgram (not b)) else receivingProgram b
--     Empty -> do (something::a) <- get'; return [something]
--
-- dabpmodel :: Choose <: f => Int -> [a] -> Free f ((),[a])
-- dabpmodel n (x:xs) = handle_ hStateS 
--                       (handle_ hStateS 
--                         (handle_ hStateS 
--                           (handle hLock 
--                             (nlockPar n (sendingProgram False xs) (receivingProgram True))) 
--                         (Switch False)) 
--                       True) 
--                     x
--
-- abpmodel :: Choose <: f => [a] -> Free f ((),[a])
-- abpmodel (x:xs) = handle_ hStateS 
--                       (handle_ hStateS 
--                         (handle_ hStateS 
--                           (handle hLock 
--                             (lockPar (sendingProgram False xs) (receivingProgram True))) 
--                         (Switch False)) 
--                       True) 
--                     x
--

data Inp = Inp Int | None -- unfortunately, did not find a way to use maybe, as state (Maybe a) would not let me put nothing.

sendingProgram :: (State Bool <: f, State (Bool, Inp) <: f, Lock <: f) => Bool -> [Int] -> Free f ()
sendingProgram b [] = do 
  unlock;lock
  (cond::Bool) <- get'
  if cond == b then do put' (not cond, None);return () else sendingProgram b []
sendingProgram b (x:xs) = do
  unlock;lock
  (cond::Bool) <- get'
  if cond == b then do put' (not cond, Inp x);sendingProgram (not b) xs else sendingProgram b (x:xs)

receivingProgram :: (Lock <: f, State Bool <: f, State (Bool, Inp) <: f) => Bool -> Free f [Int] 
receivingProgram b = do
  unlock;lock
  (cond::Bool, el :: Inp) <- get'
  case el of
    Inp something -> if cond /= b then do put' cond;fmap (something:) (receivingProgram (not b)) else receivingProgram b
    None -> return []

dabpmodel :: Choose <: f => Int -> [Int] -> Free f ((),[Int])
dabpmodel n xs = handle_ hStateS 
                  (handle_ hStateS 
                    (handle hLock 
                      (ngoesFirstLock n (sendingProgram True xs) (receivingProgram True))) 
                  True) 
                (True, None)



