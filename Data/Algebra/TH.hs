{-# LANGUAGE TemplateHaskell, TupleSections #-}
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
  , deriveSuperclassInstances
  , deriveSignature
  -- * Possibly useful internals
  , SignatureTH(..)
  , OperationTH(..)
  , SuperclassTH(..)
  , getSignatureInfo
  , buildSignatureDataType
  , signatureInstances
  ) where

import Data.Algebra.Internal

import Data.Traversable (for)
import Control.Arrow ((***))
import Data.Monoid (Endo(..))

import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (isAlpha)
import Data.List (nubBy)
import Data.Function (on)
import Language.Haskell.TH
import Data.Generics (Data, everywhere, mkT)


data SignatureTH = SignatureTH
  { signatureName :: Name
  , typeVarName :: Name
  , operations :: [OperationTH]
  , superclasses :: [SuperclassTH]
  }

data OperationTH = OperationTH
  { functionName :: Name
  , operationName :: Name
  , arity :: Int
  , constructor :: Con
  , fixity :: Fixity
  }
  
data SuperclassTH = SuperclassTH
  { superclassName :: Name
  , constrName :: Name
  , signatureTH :: SignatureTH
  }

getSignatureInfo :: Name -> Q SignatureTH
getSignatureInfo name = do
  ClassI (ClassD ctx _ [tyvar] _ decs) _ <- reify name
  let tv = tvName tyvar
  let sigName = changeName (++ "Signature") name
  ops <- for decs $ \sig ->
    case sig of
      (SigD nm (ForallT [tv'] _ tp)) -> do
        let tvn' = tvName tv'
        dec <- reify nm
        fty <- fromMaybe defaultFixity <$> reifyFixity nm
        case dec of
          ClassOpI _ _ _ ->
            return $ case buildOperation tvn' tp of
              Just (ar, mkCon) ->
                let opName = changeName addPrefix nm
                in Just $ OperationTH nm opName ar (everywhere (mkT (rename tvn' tv)) (mkCon opName)) fty
              _ -> Nothing
          _ -> fail $ "No support for " ++ show dec
      SigD{} -> fail $ "No support for " ++ show sig
      _ -> return Nothing
  scs <- for ctx $ \ty ->
    case ty of
      (AppT (ConT scName) (VarT tv')) | tv == tv' -> do
        s <- getSignatureInfo scName
        case s of
          SignatureTH _ _ [] [] -> return Nothing
          _ -> return $ Just $ SuperclassTH scName (changeName (addScPrefix name) scName) s
      _ -> return Nothing
  return $ SignatureTH sigName tv (catMaybes ops) (catMaybes scs)

-- | Derive a signature for an algebraic class.
--   For example:
--
-- > deriveSignature ''Monoid
--
--   The above would generate the following:
--
-- > data MonoidSignature a = Op_mempty | Op_mappend a a | Op_mconcat [a]
-- >   deriving (Functor, Foldable, Traversable, Eq, Ord)
-- >
-- > type instance Signature Monoid = MonoidSignature
-- >
-- > instance AlgebraSignature MonoidSignature where
-- >   type Class MonoidSignature = Monoid
-- >   evaluate Op_mempty = mempty
-- >   evaluate (Op_mappend a b) = mappend a b
-- >   evaluate (Op_mconcat ms) = mconcat ms
-- >
-- > instance Show a => Show (MonoidSignature a) where
-- >   showsPrec d Op_mempty          = showParen (d > 10) $ showString "mempty"
-- >   showsPrec d (Op_mappend a1 a2) = showParen (d > 10) $ showString "mappend" . showChar ' ' . showsPrec 11 a1 . showChar ' ' . showsPrec 11 a2
-- >   showsPrec d (Op_mconcat a1)    = showParen (d > 10) $ showString "mconcat" . showChar ' ' . showsPrec 11 a1
--
--   `deriveSignature` creates the signature data type and an instance for it of the
--   `AlgebraSignature` class. @DeriveTraversable@ is used the generate the `Traversable` instance of the signature.
--
--   This will do nothing if there is already a signature for the class in scope.
deriveSignature :: Name -> Q [Dec]
deriveSignature = fmap ((>>= snd) . nubBy ((==) `on` fst)) . deriveSignature'
  
deriveSignature' :: Name -> Q [(Name, [Dec])]
deriveSignature' className = do
  s <- getSignatureInfo className
  mName <- lookupTypeName (nameBase $ signatureName s)
  scDecs <- concat <$> traverse (deriveSignature' . superclassName) (superclasses s)
  return $ if mName == Nothing then (className, buildSignatureDataType s ++ signatureInstances className s) : scDecs else []

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

-- | Derive the instances for the superclasses too, all using the same context.
--   Usually you'd want to do this manually since you can often give a stricter context, for example:
-- 
-- > deriveSuperclassInstances [t| (Fractional m, Fractional n) => Fractional (m, n) |]
-- 
--   will derive an instance @(Fractional m, Fractional n) => Num (m, n)@ while the instance only
--   needs @(Num m, Num n)@.
deriveSuperclassInstances :: Q Type -> Q [Dec]
deriveSuperclassInstances qtyp = do
  typ <- qtyp
  case typ of
    ForallT _ ctx (AppT (ConT className) typeName) ->
      deriveSuperclassInstances' ctx className typeName
    AppT (ConT className) typeName -> 
      deriveSuperclassInstances' [] className typeName

deriveSuperclassInstances' :: Cxt -> Name -> Type -> Q [Dec]
deriveSuperclassInstances' ctx className typeName = do
  s <- getSignatureInfo className
  concatMap snd <$> deriveSuperclassInstances'' s ctx typeName id

deriveSuperclassInstances'' :: SignatureTH -> Cxt -> Type -> (Exp -> Exp) -> Q [(Name, [Dec])]
deriveSuperclassInstances'' s ctx typeName wrap =
  nubBy ((==) `on` fst) . concat <$> traverse 
    (\(SuperclassTH scName conName s') -> do
      dec <- deriveInstanceWith'' False ctx scName typeName (wrap . AppE (ConE conName)) (return [])
      scs <- deriveSuperclassInstances'' s' ctx typeName (wrap . AppE (ConE conName))
      return $ (scName, dec) :  scs)
    (superclasses s)
  
    
deriveInstanceWith' :: Bool -> Q Type -> Q [Dec] -> Q [Dec]
deriveInstanceWith' addSignature qtyp dec = do
  typ <- qtyp
  case typ of
    ForallT _ ctx (AppT (ConT className) typeName) ->
      deriveInstanceWith'' addSignature ctx className typeName id dec
    AppT (ConT className) typeName ->
      deriveInstanceWith'' addSignature [] className typeName id dec

deriveInstanceWith'' :: Bool -> Cxt -> Name -> Type -> (Exp -> Exp) -> Q [Dec] -> Q [Dec]
deriveInstanceWith'' addSignature ctx className typeName wrap dec = do
  given <- dec
  s <- getSignatureInfo className
  let
    givenLU =
      [ (nameBase nm, (nm, renamer f)) | f@(FunD nm _) <- given ] ++
      [ (nameBase nm, (nm, renamer v)) | v@(ValD (VarP nm) _ _) <- given ]
    renamer = renameAll [ (nm, nm') | (b, (nm, _)) <- givenLU, nm' <- functionName <$> operations s, nameBase nm' == b ]
    impl =
      [ maybe
          (FunD fName [Clause (map VarP args) (NormalB (AppE (VarE 'algebra) (wrap (foldl (\e arg -> AppE e (VarE arg)) (ConE opName) args)))) []])
          snd mgiven
      | OperationTH fName opName ar _ _ <- operations s, let mgiven = lookup (nameBase fName) givenLU, let args = mkArgList ar ]
  (++ [InstanceD Nothing ctx (AppT (ConT className) typeName) impl]) <$>
    if addSignature then deriveSignature className else return []

buildSignatureDataType :: SignatureTH -> [Dec]
buildSignatureDataType s =
  [DataD [] (signatureName s) [PlainTV (typeVarName s)] Nothing 
    ((constructor <$> operations s) ++ (buildSuperclassCon (typeVarName s) <$> superclasses s))
    [DerivClause Nothing (map ConT [''Functor, ''Foldable, ''Traversable, ''Eq, ''Ord])]]

signatureInstances :: Name -> SignatureTH -> [Dec]
signatureInstances nm s = [asInst, showInst, sigTFInst]
  where
    signature = ConT (signatureName s)
    sigTFInst = TySynInstD ''Signature (TySynEqn [ConT nm] signature)
    typeInst = TySynInstD ''Class (TySynEqn [signature] (ConT nm))
    asClauses =
      [ Clause [ConP opName (map VarP args)] (NormalB (foldl (\e arg -> AppE e (VarE arg)) (VarE fName) args)) []
      | OperationTH fName opName ar _ _ <- operations s, let args = mkArgList ar ]
    asScClauses = 
      [ Clause [ConP conName [(VarP v)]] (NormalB $ AppE (VarE 'evaluate) (VarE v)) []
      | SuperclassTH _ conName _ <- superclasses s, let v = mkName "v"]
    asInst = InstanceD Nothing [] (AppT (ConT ''AlgebraSignature) signature) [typeInst, FunD 'evaluate (asClauses ++ asScClauses)]
    showsPrecClauses =
      [ Clause [VarP d, ConP opName (map VarP args)] (NormalB $ createShowsPrec d (nameBase fName) prec args) []
      | OperationTH fName opName ar _ (Fixity prec _) <- operations s, let args = mkArgList ar, let d = mkName "d" ]
    showsPrecScClauses = 
      [ Clause [VarP d, ConP conName [(VarP v)]] (NormalB $ AppE (AppE (VarE 'showsPrec) (VarE d)) (VarE v)) []
      | SuperclassTH _ conName _ <- superclasses s, let d = mkName "d", let v = mkName "v"]
    createShowsPrec d name prec [u,v] | isOperator name =
      InfixE (Just (AppE (VarE 'showParen) (InfixE (Just (VarE d)) (VarE '(>)) (Just (LitE (IntegerL prec')))))) (VarE '($))
        (Just (InfixE (Just (AppE (AppE (VarE 'showsPrec) (LitE (IntegerL prec1))) (VarE u))) (VarE '(.))
        (Just (InfixE (Just (AppE (VarE 'showString) (LitE (StringL (" " ++ name ++ " "))))) (VarE '(.))
        (Just (AppE (AppE (VarE 'showsPrec) (LitE (IntegerL prec1))) (VarE v)))))))
      where
        prec' = toInteger prec
        prec1 = prec' + 1
    createShowsPrec d name _ args =
      InfixE (Just (AppE (VarE 'showParen) (InfixE (Just (VarE d)) (VarE '(>)) (Just (LitE (IntegerL 10)))))) (VarE '($)) $
        foldl addArg (Just (AppE (VarE 'showString) (LitE (StringL name)))) args
    addArg expr arg =
      Just $ InfixE expr (VarE '(.)) (Just (InfixE (Just (AppE (VarE 'showChar) (LitE (CharL ' ')))) (VarE '(.))
        (Just (AppE (AppE (VarE 'showsPrec) (LitE (IntegerL 11))) (VarE arg)))))
    showInst = InstanceD Nothing [AppT (ConT ''Show) a] 
      (AppT (ConT ''Show) (AppT signature a)) 
      [FunD 'showsPrec (showsPrecClauses ++ showsPrecScClauses)]
    a = VarT $ mkName "a"

buildOperation :: Name -> Type -> Maybe (Int, Name -> Con)
buildOperation nm (VarT nm') = if nm == nm' then Just (0, \opName -> NormalC opName []) else Nothing
buildOperation nm (AppT (AppT ArrowT h) t) = ((+1) *** fmap (prependC h)) <$> buildOperation nm t
buildOperation _ _ = Nothing

buildSuperclassCon :: Name -> SuperclassTH -> Con
buildSuperclassCon nm s = NormalC (constrName s) [(bangDef, AppT (ConT (signatureName $ signatureTH s)) (VarT nm))]

changeName :: (String -> String) -> Name -> Name
changeName f = mkName . f . nameBase

addPrefix :: String -> String
addPrefix s | isOperator s = ":%:" ++ s
addPrefix s = "Op_" ++ s

addScPrefix :: Name -> String -> String
addScPrefix nm s = "Sc_" ++ nameBase nm ++ "_" ++ s

isOperator :: String -> Bool
isOperator (c:_) = not (isAlpha c) && c /= '_'
isOperator _ = False

mkArgList :: Int -> [Name]
mkArgList n = [ mkName $ "a" ++ show i | i <- [1 .. n] ]

renameAll :: Data a => [(Name, Name)] -> a -> a
renameAll m = everywhere (mkT (appEndo (foldMap (\(a, b) -> Endo $ rename a b) m)))

rename :: Name -> Name -> Name -> Name
rename a b c | a == c = b
rename _ _ t = t

prependC :: Type -> Con -> Con
prependC st (NormalC nm sts) = NormalC nm ((bangDef, st):sts)

bangDef :: Bang
bangDef = Bang NoSourceUnpackedness NoSourceStrictness

tvName :: TyVarBndr -> Name
tvName (PlainTV nm) = nm
tvName (KindedTV nm _) = nm
