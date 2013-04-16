{-# LANGUAGE 
    TypeFamilies 
  , ConstraintKinds
  , MultiParamTypeClasses
  , FlexibleInstances
  , UndecidableInstances
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
    -- * Template Haskell functions
  , deriveInstance
  , deriveSignature
    -- * Example signature
  , MonoidSignature(..)
  ) where

import Data.Algebra.Internal
import Data.Algebra.TH

import Control.Arrow ((&&&))
import Data.Monoid
import Data.Foldable
import Data.Traversable


instance Algebra f () where
  algebra = const () 

instance (Class f m, Class f n) => Algebra f (m, n) where
  algebra = evaluate . fmap fst &&& evaluate . fmap snd

instance Class f b => Algebra f (a -> b) where
  algebra fab a = evaluate (fmap ($ a) fab)
  
-- | The `Monoid` signature has this `AlgebraSignature` instance:
--
-- > instance AlgebraSignature MonoidSignature where
-- >   type Class MonoidSignature = Monoid
-- >   evaluate Op_mempty = mempty
-- >   evaluate (Op_mappend a b) = mappend a b
-- >   evaluate (Op_mconcat ms) = mconcat ms
deriveSignature ''Monoid