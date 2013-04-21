{-# LANGUAGE 
    TypeFamilies 
  , ConstraintKinds
  , MultiParamTypeClasses
  , TemplateHaskell
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algebra
-- Copyright   :  (c) Sjoerd Visscher 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Data.Algebra 
  ( -- * Classes
    AlgebraSignature(..)
  , Algebra(..)
  , algebraA
    -- * Template Haskell functions
  , deriveInstance
  , deriveSignature
    -- * Example signature
  , MonoidSignature(..)
  ) where

import Data.Algebra.Internal
import Data.Algebra.TH

import Data.Monoid

  
-- | The `Monoid` signature has this `AlgebraSignature` instance:
--
-- > instance AlgebraSignature MonoidSignature where
-- >   type Class MonoidSignature = Monoid
-- >   evaluate Op_mempty = mempty
-- >   evaluate (Op_mappend a b) = mappend a b
-- >   evaluate (Op_mconcat ms) = mconcat ms
deriveSignature ''Monoid