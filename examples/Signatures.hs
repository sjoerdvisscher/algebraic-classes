{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveTraversable #-}

import Data.Algebra.TH

deriveSignature ''Monoid
deriveSignature ''Num
