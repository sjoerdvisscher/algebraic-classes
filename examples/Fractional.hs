{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Algebra
import Data.Algebra.TH
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Data.Ratio

deriveSignature ''Num
deriveSignature ''Fractional

deriveInstance [t| (Num m, Num n) => Num (m, n) |]
deriveInstance [t| (Fractional m, Fractional n) => Fractional (m, n) |]

test :: (Ratio Int, Ratio Int)
test = (5, 3) / (3, 2) + (1, 4)