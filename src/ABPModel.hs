{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE IncoherentInstances #-}

module ABPModel (
  ) where

import Lib
import HigherOrder
import Choose
import Conc
import Data.Tuple (swap)
import State


startSending :: [a] -> Free f ()
startSending = sendingProgram True

sendingProgram :: (State Bool <: f, State (Bool, a)) => Bool -> [a] -> Free f ()
sendingProgram _ [] = Pure ()
sendingProgram (x:xs) = do
  put' (True, x)

