{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Algebra.TH
-- Copyright   :  (c) Sjoerd Visscher 2013
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Data.Algebra.TH 
  ( deriveInstance
  , deriveSignature
  -- * Possibly useful internals
  , SignatureTH(..)
  , OperationTH(..)
  , getSignatureInfo
  , buildSignatureDataType
  , signatureInstance
  ) where

import Data.Algebra.Internal

import Control.Applicative
import Control.Arrow ((***))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Language.Haskell.TH
import Data.Generics


data SignatureTH = SignatureTH 
  { signatureName :: Name
  , typeVarName :: Name
  , operations :: [OperationTH]
  }

data OperationTH = OperationTH
  { functionName :: Name
  , operationName :: Name
  , arity :: Int
  , constructor :: Con
  }

getSignatureInfo :: Name -> Q SignatureTH
getSignatureInfo name = do
  ClassI (ClassD _ _ _ _ decs) _ <- reify name
  let tv = mkName "a"
  let sigName = changeName (++ "Signature") name 
  return 
    $ SignatureTH sigName tv
      [ OperationTH nm opName ar (everywhere (mkT (rename tv' tv)) (mkCon opName))
      | SigD nm (ForallT [PlainTV tv'] _ tp) <- decs 
      , Just (ar, mkCon) <- [buildOperation tv' tp]
      , let opName = changeName ("Op_" ++) nm
      ]

-- | Derive a signature for an algebraic class.
--   For exaple:
--
-- > deriveSignature ''Num
--
--   `deriveSignature` creates the signature data type and an instance for it of the
--   `AlgebraSignature` class. @DeriveFunctor@ is used the generate the `Functor` instance of the signature.
--
--   This will do nothing if there is already a signature for the class in scope.
deriveSignature :: Name -> Q [Dec]
deriveSignature className = do
  mName <- lookupTypeName (nameBase className ++ "Signature")
  s <- getSignatureInfo className
  return $ if mName == Nothing then buildSignatureDataType s ++ signatureInstance className s else []

-- | Derive an instance for an algebraic class.
--   For example: 
--
--   > deriveInstance [t| (Num m, Num n) => Num (m, n) |]
--
--   To be able to derive an instance for @a@ of class @c@, we need an instance of @`Algebra` f a@,
--   where @f@ is the signature of @c@.
--
--   `deriveInstance` will generate a signature for the class if there is no signature in scope.
deriveInstance :: Q Type -> Q [Dec]
deriveInstance typ = do
  (ForallT _ ctx (AppT (ConT className) typeName)) <- typ
  s <- getSignatureInfo className
  let
    impl = 
      [ FunD fName [Clause (map VarP args) (NormalB (AppE (VarE 'algebra) (foldl (\e arg -> AppE e (VarE arg)) (ConE opName) args))) []] 
      | OperationTH fName opName ar _ <- operations s, let args = mkArgList ar ]
  (++ [InstanceD ctx (AppT (ConT className) typeName) impl]) <$> deriveSignature className

buildSignatureDataType :: SignatureTH -> [Dec]
buildSignatureDataType s =
  let cons = [ con | OperationTH _ _ _ con <- operations s ]
  in [DataD [] (signatureName s) [PlainTV (typeVarName s)] cons [''Functor, ''Foldable, ''Traversable, ''Show]]

signatureInstance :: Name -> SignatureTH -> [Dec]
signatureInstance nm s = [inst]
  where
    typeInst = TySynInstD ''Class [ConT (signatureName s)] (ConT nm)
    clauses = 
      [ Clause [ConP opName (map VarP args)] (NormalB (foldl (\e arg -> AppE e (VarE arg)) (VarE fName) args)) []
      | OperationTH fName opName ar _ <- operations s, let args = mkArgList ar ]
    inst = InstanceD [] (AppT (ConT ''AlgebraSignature) (ConT (signatureName s))) [typeInst, FunD 'evaluate clauses]

buildOperation :: Name -> Type -> Maybe (Int, Name -> Con)
buildOperation nm (VarT nm') = if nm == nm' then Just (0, \opName -> NormalC opName []) else Nothing
buildOperation nm (AppT (AppT ArrowT h) t) = ((+1) *** fmap (prependC (NotStrict, h))) <$> buildOperation nm t
buildOperation _ t = Nothing

changeName :: (String -> String) -> Name -> Name
changeName f = mkName . f . nameBase

mkArgList :: Int -> [Name]
mkArgList n = [ mkName $ "a" ++ show i | i <- [1 .. n] ]

rename :: Name -> Name -> Type -> Type
rename a b (VarT c) | a == c = VarT b
rename _ _ t = t

prependC :: (Strict, Type) -> Con -> Con
prependC st (NormalC nm sts) = NormalC nm (st:sts)