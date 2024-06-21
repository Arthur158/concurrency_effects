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



