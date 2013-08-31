{-# LANGUAGE 
    TypeFamilies 
  , ConstraintKinds
  , MultiParamTypeClasses
  , FlexibleInstances
  , UndecidableInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algebra.Internal
-- Copyright   :  (c) Sjoerd Visscher 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Data.Algebra.Internal where

import GHC.Exts (Constraint)
import Control.Applicative
import Data.Traversable (Traversable(..))

import GHC.Conc (STM)
import Data.Monoid
import Control.Arrow ((&&&))

-- | The signature datatype for the class @c@.
type family Signature (c :: * -> Constraint) :: * -> *

class Traversable f => AlgebraSignature f where
  -- | The class for which @f@ is the signature.
  type Class f :: * -> Constraint
  -- | Translate the operations of the signature to method calls of the class.
  evaluate :: Class f b => f b -> b

class Algebra f a where
  -- | An algebra @f a -> a@ corresponds to an instance of @a@ of the class @Class f@.
  --   In some cases, for example for tuple types, you can give an algebra generically for every signature:
  --
  -- > instance (Class f m, Class f n) => Algebra f (m, n) where
  -- >   algebra fmn = (evaluate (fmap fst fmn), evaluate (fmap snd fmn))
  algebra :: AlgebraSignature f => f a -> a
  
-- | If you just want to applicatively lift existing instances, you can use this default implementation of `algebra`.
algebraA :: (Applicative g, Class f b, AlgebraSignature f) => f (g b) -> g b
algebraA = fmap evaluate . sequenceA

instance Algebra f () where
  algebra = const () 
instance (Class f m, Class f n) => Algebra f (m, n) where
  algebra = evaluate . fmap fst &&& evaluate . fmap snd

instance Class f b => Algebra f (a -> b) where algebra = algebraA
instance Class f b => Algebra f (IO b) where algebra = algebraA
instance Class f b => Algebra f (Maybe b) where algebra = algebraA
instance Class f b => Algebra f (Either a b) where algebra = algebraA
instance Class f b => Algebra f (STM b) where algebra = algebraA
instance (Monoid m, Class f b) => Algebra f (Const m b) where algebra = algebraA
