{-# OPTIONS_GHC -ddump-splices -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveTraversable, RankNTypes #-}

import Data.Algebra
-- import Data.Algebra.TH
import Data.Ratio

deriveInstance [t| forall m n. (Num m, Num n) => Num (m, n) |]
deriveInstance [t| forall m n. (Fractional m, Fractional n) => Fractional (m, n) |]
-- deriveSuperclassInstances [t| forall m n. (Fractional m, Fractional n) => Fractional (m, n) |]

test :: (Ratio Int, Ratio Int)
test = (5, 3) / (3, 2) + (1, 4)
