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
  , deriveInstanceWith_skipSignature
  , deriveSignature
  -- * Possibly useful internals
  , SignatureTH(..)
  , OperationTH(..)
  , getSignatureInfo
  , buildSignatureDataType
  , signatureInstances
  ) where

import Data.Algebra.Internal

import Control.Applicative
import Control.Arrow ((***))
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable, forM)
import Data.Monoid (Endo(..))
import Data.Maybe (catMaybes)

import Language.Haskell.TH
import Data.Generics (Data, everywhere, mkT)


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
  , fixity :: Fixity
  }

getSignatureInfo :: Name -> Q SignatureTH
getSignatureInfo name = do
  ClassI (ClassD _ _ _ _ decs) _ <- reify name
  let tv = mkName "a"
  let sigName = changeName (++ "Signature") name
  ops <- forM decs $ \(SigD nm (ForallT [PlainTV tv'] _ tp)) -> do
    ClassOpI _ _ _ fty <- reify nm
    return $ case buildOperation tv' tp of
      Just (ar, mkCon) -> 
        let opName = changeName ("Op_" ++) nm
        in Just $ OperationTH nm opName ar (everywhere (mkT (rename tv' tv)) (mkCon opName)) fty
      _ -> Nothing
  return $ SignatureTH sigName tv $ catMaybes ops

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
  return $ if mName == Nothing then buildSignatureDataType s ++ signatureInstances className s else []

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
deriveInstanceWith = deriveInstanceWith' True

-- | Derive an instance for an algebraic class with a given partial implementation,
--   but don't generate the signature. This is for when you want to derive several instances
--   of the same class, but can't splice the results directly. In that case 'deriveSignature'
--   can't detect it has already generated the signature earlier.
deriveInstanceWith_skipSignature :: Q Type -> Q [Dec] -> Q [Dec]
deriveInstanceWith_skipSignature = deriveInstanceWith' False

deriveInstanceWith' :: Bool -> Q Type -> Q [Dec] -> Q [Dec]
deriveInstanceWith' addSignature qtyp dec = do
  typ <- qtyp
  case typ of
    ForallT _ ctx (AppT (ConT className) typeName) -> 
      deriveInstanceWith'' addSignature ctx className typeName dec
    AppT (ConT className) typeName -> 
      deriveInstanceWith'' addSignature [] className typeName dec

deriveInstanceWith'' :: Bool -> Cxt -> Name -> Type -> Q [Dec] -> Q [Dec]
deriveInstanceWith'' addSignature ctx className typeName dec = do
  given <- dec
  s <- getSignatureInfo className
  let 
    givenLU = 
      [ (nameBase nm, (nm, renamer f)) | f@(FunD nm _) <- given ] ++ 
      [ (nameBase nm, (nm, renamer v)) | v@(ValD (VarP nm) _ _) <- given ]
    renamer = renameAll [ (nm, nm') | (b, (nm, _)) <- givenLU, nm' <- functionName <$> operations s, nameBase nm' == b ]
    impl = 
      [ maybe 
          (FunD fName [Clause (map VarP args) (NormalB (AppE (VarE 'algebra) (foldl (\e arg -> AppE e (VarE arg)) (ConE opName) args))) []]) 
          snd mgiven
      | OperationTH fName opName ar _ _ <- operations s, let mgiven = lookup (nameBase fName) givenLU, let args = mkArgList ar ]   
  (++ [InstanceD ctx (AppT (ConT className) typeName) impl]) <$> 
    if addSignature then deriveSignature className else return []

buildSignatureDataType :: SignatureTH -> [Dec]
buildSignatureDataType s =
  [DataD [] (signatureName s) [PlainTV (typeVarName s)] (constructor <$> operations s)
    [''Functor, ''Foldable, ''Traversable, ''Eq, ''Ord]]

signatureInstances :: Name -> SignatureTH -> [Dec]
signatureInstances nm s = [asInst, showInst, sigTFInst]
  where
    signature = ConT (signatureName s)
    sigTFInst = TySynInstD ''Signature [ConT nm] signature
    typeInst = TySynInstD ''Class [signature] (ConT nm)
    asClauses = 
      [ Clause [ConP opName (map VarP args)] (NormalB (foldl (\e arg -> AppE e (VarE arg)) (VarE fName) args)) []
      | OperationTH fName opName ar _ _ <- operations s, let args = mkArgList ar ]
    asInst = InstanceD [] (AppT (ConT ''AlgebraSignature) signature) [typeInst, FunD 'evaluate asClauses]
    showsPrecClauses =
      [ Clause [VarP d, ConP opName (map VarP args)] (NormalB $ createShowsPrec d (nameBase fName) prec args) []
      | OperationTH fName opName ar _ (Fixity prec _) <- operations s, let args = mkArgList ar, let d = mkName "d" ]
    createShowsPrec d name prec [u,v] | prec < 10 = 
      InfixE (Just (AppE (VarE 'showParen) (InfixE (Just (VarE d)) (VarE '(>)) (Just (LitE (IntegerL prec')))))) (VarE '($)) 
        (Just (InfixE (Just (AppE (AppE (VarE 'showsPrec) (LitE (IntegerL prec1))) (VarE u))) (VarE '(.)) 
        (Just (InfixE (Just (AppE (VarE 'showString) (LitE (StringL (" " ++ name ++ " "))))) (VarE '(.)) 
        (Just (AppE (AppE (VarE 'showsPrec) (LitE (IntegerL prec1))) (VarE v)))))))
      where
        prec' = toInteger prec
        prec1 = prec' + 1
    createShowsPrec d name prec args = 
      InfixE (Just (AppE (VarE 'showParen) (InfixE (Just (VarE d)) (VarE '(>)) (Just (LitE (IntegerL 10)))))) (VarE '($)) $
        foldr addArg (Just (AppE (VarE 'showString) (LitE (StringL name)))) args
    addArg arg expr = 
      Just $ InfixE expr (VarE '(.)) (Just (InfixE (Just (AppE (VarE 'showChar) (LitE (CharL ' ')))) (VarE '(.)) 
        (Just (AppE (AppE (VarE 'showsPrec) (LitE (IntegerL 11))) (VarE arg)))))
    showInst = InstanceD [ClassP ''Show [a]] (AppT (ConT ''Show) (AppT signature a)) [FunD 'showsPrec showsPrecClauses]
    a = VarT $ mkName "a"

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