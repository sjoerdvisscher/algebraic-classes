{-# LANGUAGE 
    TypeFamilies 
  , ConstraintKinds
  , MultiParamTypeClasses
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
import Data.Traversable (Traversable)

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
