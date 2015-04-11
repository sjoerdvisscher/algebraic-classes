{-# LANGUAGE
    TypeFamilies
  , ConstraintKinds
  , MultiParamTypeClasses
  , TemplateHaskell
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
  ( deriveInstance
  , deriveInstanceWith
    -- * Classes
  , Algebra(..)
  , algebraA
  , Signature
  , AlgebraSignature(..)
  ) where

import Data.Algebra.Internal
import Data.Algebra.TH
