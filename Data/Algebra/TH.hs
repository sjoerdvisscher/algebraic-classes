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
  , arguments :: [Type]
  }

getSignatureInfo :: Name -> Q SignatureTH
getSignatureInfo name = do
  ClassI (ClassD _ _ _ _ decs) _ <- reify name
  let tv = mkName "a"
  SignatureTH 
    <$> changeName (++ "Signature") name 
    <*> pure tv 
    <*> sequence 
      [ OperationTH nm
        <$> changeName ("Op_" ++) nm 
        <*> (everywhere (mkT (rename tv' tv)) <$> buildOperation tv' tp)
      | SigD nm (ForallT [PlainTV tv'] _ tp) <- decs 
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
      | OperationTH fName opName ts <- operations s, let args = mkArgList (length ts) ]
  (++ [InstanceD ctx (AppT (ConT className) typeName) impl]) <$> deriveSignature className

buildSignatureDataType :: SignatureTH -> [Dec]
buildSignatureDataType s =
  let cons = [ NormalC nm (map ((,) NotStrict) ts) | OperationTH _ nm ts <- operations s ]
  in [DataD [] (signatureName s) [PlainTV (typeVarName s)] cons [''Functor, ''Foldable, ''Traversable, ''Show]]

signatureInstance :: Name -> SignatureTH -> [Dec]
signatureInstance nm s = [inst]
  where
    typeInst = TySynInstD ''Class [ConT (signatureName s)] (ConT nm)
    clauses = 
      [ Clause [ConP opName (map VarP args)] (NormalB (foldl (\e arg -> AppE e (VarE arg)) (VarE fName) args)) []
      | OperationTH fName opName ts <- operations s, let args = mkArgList (length ts) ]
    inst = InstanceD [] (AppT (ConT ''AlgebraSignature) (ConT (signatureName s))) [typeInst, FunD 'evaluate clauses]

buildOperation :: Name -> Type -> Q [Type]
buildOperation nm (VarT nm') = if nm == nm' then return [] else fail "This class is not an algebra."
buildOperation nm (AppT (AppT ArrowT h) t) = (h :) <$> buildOperation nm t
buildOperation _ t = fail $ "Don't know how to handle: " ++ show t

changeName :: (String -> String) -> Name -> Q Name
changeName f = return . mkName . f . nameBase

mkArgList :: Int -> [Name]
mkArgList n = [ mkName $ "a" ++ show i | i <- [1 .. n] ]

rename :: Name -> Name -> Type -> Type
rename a b (VarT c) | a == c = VarT b
rename _ _ t = t