 {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Algebra
import Data.Algebra.TH
import Data.Monoid
import Data.Foldable
import Data.Traversable

deriveSignature ''Monoid
deriveSignature ''Num
