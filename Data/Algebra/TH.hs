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
  , deriveInstanceWith
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
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable)
import Data.Monoid (Endo(..))

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
--   For example:
--
-- > deriveSignature ''Monoid
--
--   The above would generate the following:
--
-- > data MonoidSignature a = Op_mempty | Op_mappend a a | Op_mconcat [a]
-- >   deriving (Functor, Foldable, Traversable, Show, Eq, Ord)
-- > instance AlgebraSignature MonoidSignature where
-- >   type Class MonoidSignature = Monoid
-- >   evaluate Op_mempty = mempty
-- >   evaluate (Op_mappend a b) = mappend a b
-- >   evaluate (Op_mconcat ms) = mconcat ms  
--
--   `deriveSignature` creates the signature data type and an instance for it of the
--   `AlgebraSignature` class. @DeriveTraversable@ is used the generate the `Traversable` instance of the signature.
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
deriveInstance typ = deriveInstanceWith typ $ return []

-- | Derive an instance for an algebraic class with a given partial implementation.
--   For example:
--
-- > deriveInstanceWith [t| Num n => Num (Integer -> n) |] 
-- >   [d|
-- >     fromInteger x y = fromInteger (x + y)
-- >   |]
deriveInstanceWith :: Q Type -> Q [Dec] -> Q [Dec]
deriveInstanceWith qtyp dec = do
  typ <- qtyp
  case typ of
    ForallT _ ctx (AppT (ConT className) typeName) -> deriveInstanceWith' ctx className typeName dec
    AppT (ConT className) typeName -> deriveInstanceWith' [] className typeName dec

deriveInstanceWith' :: Cxt -> Name -> Type -> Q [Dec] -> Q [Dec]
deriveInstanceWith' ctx className typeName dec = do
  given <- dec
  s <- getSignatureInfo className
  let 
    givenLU = 
      [ (nameBase nm, (nm, renamer f)) | f@(FunD nm _) <- given ] ++ 
      [ (nameBase nm, (nm, renamer v)) | v@(ValD (VarP nm) _ _) <- given ]
    renamer = renameAll [ (nm, nm') | (b, (nm, _)) <- givenLU, OperationTH nm' _ _ _ <- operations s, nameBase nm' == b ]
    impl = 
      [ maybe 
          (FunD fName [Clause (map VarP args) (NormalB (AppE (VarE 'algebra) (foldl (\e arg -> AppE e (VarE arg)) (ConE opName) args))) []]) 
          snd mgiven
      | OperationTH fName opName ar _ <- operations s, let mgiven = lookup (nameBase fName) givenLU, let args = mkArgList ar ]   
  (++ [InstanceD ctx (AppT (ConT className) typeName) impl]) <$> deriveSignature className

buildSignatureDataType :: SignatureTH -> [Dec]
buildSignatureDataType s =
  let cons = [ con | OperationTH _ _ _ con <- operations s ]
  in [DataD [] (signatureName s) [PlainTV (typeVarName s)] cons [''Functor, ''Foldable, ''Traversable, ''Show, ''Eq, ''Ord]]

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
buildOperation _ _ = Nothing

changeName :: (String -> String) -> Name -> Name
changeName f = mkName . f . nameBase

mkArgList :: Int -> [Name]
mkArgList n = [ mkName $ "a" ++ show i | i <- [1 .. n] ]

renameAll :: Data a => [(Name, Name)] -> a -> a
renameAll m = everywhere (mkT (appEndo (foldMap (\(a, b) -> Endo $ rename a b) m)))

rename :: Name -> Name -> Name -> Name
rename a b c | a == c = b
rename _ _ t = t

prependC :: (Strict, Type) -> Con -> Con
prependC st (NormalC nm sts) = NormalC nm (st:sts)