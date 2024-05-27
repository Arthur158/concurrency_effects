{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module ConcEffect (
  ) where

import Lib
import HigherOrder
import Choose

data Conc j k = Par j k
  deriving Functor

-- par' :: Conc a <: f => Free f a -> Free f ()
-- par' k = Op (inj' (Par k (Pure ())))


-- choose :: Choose <: f => Free f Bool
-- choose = Op (inj' (Choose Pure))
