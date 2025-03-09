{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- Implementation of Hindley Milner algorithm W
-- to system F translation by Ryan Wisnesky, with extensions for Hydra by Joshua Shinavier
-- License: Apache 2.0 https://www.apache.org/licenses/LICENSE-2.0

module Hydra.Staging.Inference.AlgorithmW where

import Hydra.Minimal

import Prelude
import qualified Control.Monad as CM
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Debug.Trace
import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as Y


natType = TyLit $ LiteralTypeInteger IntegerTypeInt32
constNeg = ExprConst $ PrimTyped $ TypedPrimitive (Name "hydra.lib.math.neg") $ TypSch [] $ TyFn natType natType
-- Note: Hydra has no built-in pred or succ functions, but neg has the expected type
constPred = constNeg
constSucc = constNeg
nat = ExprConst . PrimLiteral . int32
str = ExprConst . PrimLiteral . string

-- A typed primitive corresponds to the Hydra primitive of the same name
data TypedPrimitive = TypedPrimitive Name TypSch deriving (Eq, Show)


------------------------
-- STLC

type Var = String

data Prim
 = PrimLiteral Literal
 | PrimTyped TypedPrimitive
 | PrimIf
 | PrimFst | PrimSnd | PrimPair | PrimTT
 | PrimNil | PrimCons | FoldList
 | PrimFF | PrimInl | PrimInr | PrimCase | PrimCon Var | PrimFold Var
 deriving Eq

instance Show Prim where
  show (PrimLiteral l) = case l of
    LiteralBoolean b -> show b
    LiteralFloat fv -> case fv of
      FloatValueBigfloat f -> show f
      FloatValueFloat32 f -> show f
      _ -> show fv
    LiteralInteger iv -> case iv of
      IntegerValueInt32 i -> show i
      _ -> show iv
    LiteralString s -> show s
    _ -> show l
  show (PrimTyped (TypedPrimitive name _)) = unName name ++ "()"
  show FoldList = "fold"
  show PrimFst = "fst"
  show PrimSnd = "snd"
  show PrimNil = "nil"
  show PrimCons = "cons"
  show PrimTT = "tt"
  show PrimFF = "ff"
  show PrimInl = "inl"
  show PrimInr = "inr"
  show PrimCase = "case"
  show PrimIf = "if0"
  show PrimPair = "pair"
  show (PrimCon  n) = n
  show (PrimFold n) = "fold_" ++ n

data Expr
 = ExprConst Prim
 | ExprVar Var
 | ExprTuple [Expr]
 | ExprProj Int Int Expr
 | ExprInj Int Int Expr
 | ExprCase Expr [Expr]
 | ExprApp Expr Expr
 | ExprAbs Var Expr
 | ExprLetrec [(Var, Expr)] Expr
 deriving Eq

instance Show Expr where
  show (ExprCase t ts) = "case " ++ show t ++ " of " ++ x ++ " end"
   where x = foldl (\p q -> p ++ "\n\t\t" ++ q) "" $ map show ts
  show (ExprProj i j e) = show e ++ "." ++ show i ++ " (arity " ++ show j ++ ")"
  show (ExprInj i j e) = "inj " ++ show i ++ " (arity " ++ show j ++ ") " ++ show e
  show (ExprTuple ts) = "<" ++ show (map show ts) ++ ">"
  show (ExprConst p) = show p
  show (ExprVar v) = v
  show (ExprApp (ExprApp (ExprApp a' a) b) b') = "(" ++ show a' ++ " " ++ show a ++ " " ++ show b ++ " " ++ show b' ++ ")"
  show (ExprApp (ExprApp a b) b') = "(" ++ show a ++ " " ++ show b ++ " " ++ show b' ++ ")"
  show (ExprApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (ExprAbs a b) = "(\\" ++ a ++ ". " ++ show b ++ ")"
  show (ExprLetrec ab c) = "letrec " ++ d ++ show c
    where d = foldr (\(p, q) r -> p ++ " = " ++ show q ++ " \n\t\t" ++ r) "in " ab

data MTy
  = TyVar Var
  | TyLit LiteralType
  | TyList MTy
  | TyFn MTy MTy
  | TyProd MTy MTy
  | TySum MTy MTy
  | TyUnit
  | TyVoid
  | TyTuple [MTy]
  | TyVariant [MTy]
  | TyCon Var [MTy]
 deriving Eq

instance Show MTy where
  show (TyLit lt) = case lt of
    LiteralTypeInteger it -> drop (length "IntegerType") $ show it
    LiteralTypeFloat ft -> drop (length "FloatType") $ show ft
    _ -> drop (length "LiteralType") $ show lt
  show (TyTuple ts) = "(Tuple " ++ show (map show ts) ++ ")"
  show (TyVariant ts) = "(Variant " ++ show (map show ts) ++ ")"
  show (TyVar v) = v
  show (TyList t) = "(List " ++ (show t) ++ ")"
  show (TyFn t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TyProd t1 t2) = "(" ++ show t1 ++ " * " ++ show t2 ++  ")"
  show (TySum t1 t2) = "(" ++ show t1 ++ " + " ++ show t2 ++  ")"
  show TyUnit = "Unit"
  show TyVoid = "Void"
  show (TyCon c ts) = c ++ " " ++ ts'
   where ts' = foldr (\p r -> show p ++ "" ++ r) "" ts

data TypSch = TypSch {
  typSchVariables ::  [Var],
  typSchTypes :: MTy} deriving Eq

instance Show TypSch where
  show (TypSch [] t) = show t
  show (TypSch x t) = "forall " ++ d ++ show t
   where d = foldr (\p q ->  p ++ " " ++ q) ", " x

type ADTs = [(Var, [Var], [(Var, [MTy])])]

------------------------
-- System F

data FExpr
 = FConst Prim
 | FVar Var
 | FTuple [FExpr]
 | FProj Int FExpr
 | FInj Int [FTy] FExpr
 | FCase FExpr FTy [FExpr]
 | FApp FExpr FExpr
 | FAbs Var FTy FExpr
 | FTyApp FExpr [FTy]
 | FTyAbs [Var] FExpr
 | FLetrec [(Var, FTy, FExpr)] FExpr
 deriving Eq

instance Show FExpr where
  show (FCase t t' []) = "ff " ++ show t ++ " " ++ show t' ++ " "
  show (FCase t t' ts) = "case " ++ show t ++ " of " ++ show (map show ts) ++ " end"
  show (FProj i e) = show e ++ "." ++ show i
  show (FInj i j e) = "inj " ++ show i ++ " (arity " ++ show j ++ ") " ++ show e
  show (FTuple es) = "<" ++ show (map show es) ++ ">"
  show (FConst p) = show p
  show (FVar v) = v
  show (FTyApp e t) = "(" ++ show e ++ " " ++ show t ++ ")"
  show (FApp (FApp (FApp a' a) b) b') = "(" ++ show a' ++ " " ++ show a ++ " " ++ show b ++ " " ++ show b' ++ ")"
  show (FApp (FApp a b) b') = "(" ++ show a ++ " " ++ show b ++ " " ++ show b' ++ ")"
  show (FApp a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (FAbs a t b) = "(\\" ++ a ++ ":" ++ show t ++ ". " ++ show b ++ ")"
  show (FLetrec ab c) = "letrecs " ++ d ++ show c
    where d = foldr (\(p, t, q) r -> p ++ ":" ++ show t ++ " = " ++ show q ++ " \n\t\t" ++ r) "in " ab
  show (FTyAbs ab c) = "(/\\" ++ d ++ show c ++ ")"
    where d = foldr (\p r -> p ++ " " ++ r) ". " ab

data FTy = FTyVar Var
  | FTyLit LiteralType
  | FTyList FTy
  | FTyFn FTy FTy
  | FTyProd FTy FTy
  | FTySum FTy FTy
  | FTyUnit
  | FTyVoid
  | FTyTuple [FTy]
  | FTyVariant [FTy]
  | FTyCon Var [FTy]
  | FTyForall [Var] FTy
 deriving Eq

instance Show FTy where
  show (FTyLit lt) = show $ TyLit lt
  show (FTyVariant ts) = "(Variant " ++ show (map show ts) ++ ")"
  show (FTyTuple ts) = "(Tuple " ++ show (map show ts) ++ ")"
  show (FTyVar v) = v
  show (FTyList t) = "(List " ++ (show t) ++ ")"
  show (FTyFn t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (FTyProd t1 t2) = "(" ++ show t1 ++ " * " ++ show t2 ++  ")"
  show (FTySum t1 t2) = "(" ++ show t1 ++ " + " ++ show t2 ++  ")"
  show FTyUnit = "Unit"
  show FTyVoid = "Void"
  show (FTyForall x t) = "(forall " ++ d ++ show t ++ ")"
   where d = foldr (\p q -> p ++ " " ++ q) ", " x
  show (FTyCon c ts) = c ++ " " ++ ts'
   where ts' = foldr (\p r -> show p ++ "" ++ r) " " ts

mTyToFTy :: MTy -> FTy
mTyToFTy (TyVar v) = FTyVar v
mTyToFTy (TyLit lt) = FTyLit lt
mTyToFTy TyUnit = FTyUnit
mTyToFTy TyVoid = FTyVoid
mTyToFTy (TyList x) = FTyList $ mTyToFTy x
mTyToFTy (TyFn x y) = FTyFn (mTyToFTy x) (mTyToFTy y)
mTyToFTy (TyProd x y) = FTyProd (mTyToFTy x) (mTyToFTy y)
mTyToFTy (TySum x y) = FTySum (mTyToFTy x) (mTyToFTy y)
mTyToFTy (TyTuple ts) = FTyTuple (map mTyToFTy ts)
mTyToFTy (TyVariant ts) = FTyVariant (map mTyToFTy ts)
mTyToFTy (TyCon c ts) = FTyCon c $ map mTyToFTy ts

tyToFTy :: TypSch -> FTy
tyToFTy (TypSch [] t) = mTyToFTy t
tyToFTy (TypSch vs t) = FTyForall vs (mTyToFTy t)

--------------------
-- Contexts

type Ctx = M.Map Var TypSch

instance Show Ctx where
  show ctx = case M.toList ctx of
    [] -> ""
    ((k,v):t) -> k ++ ":" ++ show v ++ " " ++ show t

variablesInContext :: Ctx -> [Var]
variablesInContext ctx = L.concat $ fmap variablesInTypeScheme $ M.elems ctx

variablesInType :: MTy -> [Var]
variablesInType t = case t of
 TyCon _ x -> variablesInType $ TyVariant x --cheat
 TyFn t1 t2 -> variablesInType t1 ++ variablesInType t2
 TyList t -> variablesInType t
 TyLit _ -> []
 TyProd t1 t2 -> variablesInType t1 ++ variablesInType t2
 TySum t1 t2 -> variablesInType t1 ++ variablesInType t2
 TyTuple (a:b) -> variablesInType a ++ variablesInType (TyTuple b)
 TyTuple [] -> []
 TyUnit -> []
 TyVar v -> [v]
 TyVariant (a:b) -> variablesInType a ++ variablesInType (TyVariant b)
 TyVariant [] -> []
 TyVoid -> []

variablesInTypeScheme :: TypSch -> [Var]
variablesInTypeScheme (TypSch vs t) = filter (\v -> not $ elem v vs) (variablesInType t)

replaceTCon :: MTy -> MTy -> MTy -> MTy
replaceTCon u s (TyVar x) = TyVar x
replaceTCon u s (TyLit l) = TyLit l
replaceTCon u s (TyList t)  = TyList $ replaceTCon u s t
replaceTCon u s (TyFn t1 t2)  = TyFn (replaceTCon u s t1) (replaceTCon u s t2)
replaceTCon u s TyUnit  = TyUnit
replaceTCon u s TyVoid  = TyVoid
replaceTCon u s (TyProd t1 t2)  = TyProd (replaceTCon u s t1) (replaceTCon u s t2)
replaceTCon u s (TySum t1 t2)  = TySum (replaceTCon u s t1) (replaceTCon u s t2)
replaceTCon u s (TyTuple [])  = TyTuple []
replaceTCon u s (TyTuple (a:b)) = TyTuple $ (replaceTCon u s a):(map (replaceTCon u s) b)
replaceTCon u s (TyVariant [])  = TyVariant []
replaceTCon u s (TyVariant (a:b)) = TyVariant $ (replaceTCon u s a):(map (replaceTCon u s) b)
replaceTCon u s (TyCon t' ts)  | TyCon t' ts == u = s
                               | otherwise = TyCon t' $ map (replaceTCon u s) ts

primTy :: ADTs -> Prim -> Either String TypSch
primTy _ (PrimLiteral l) = Right $ TypSch [] $ TyLit $ literalType l
primTy _ (PrimTyped (TypedPrimitive _ forAll)) = Right forAll
primTy [] (PrimCon n) = throwError $ n ++ " not found "
primTy ((a,t,[]):tl) (PrimCon n) = primTy tl $ PrimCon n
primTy ((a,t,(c,ts):cs):tl) (PrimCon n) | c == n = return $ TypSch t (ts' ts $ TyCon a $ map TyVar t)
                                    | otherwise = primTy ((a,t,cs):tl) $ PrimCon n
 where ts' [] x = x
       ts' (p:q) x = TyFn p $ ts' q x
primTy [] (PrimFold n) = throwError $ n ++ " not found "
primTy ((a,t,cs):tl) (PrimFold n) | a == n = return $ TypSch ("r":t) $ elimTy
                              | otherwise = primTy tl $ PrimFold n
 where elimTy = TyFn (TyCon a $ map TyVar t) $ replaceTCon (TyCon a $ map TyVar t) (TyVar "r") (ts' cs)
       ts' [] = TyVar "r"
       ts' ((_,ts):q)  = TyFn (ts'' ts) $ ts' q
       ts'' [] = TyCon a $ map TyVar t
       ts'' (a:b) = TyFn a $ ts'' b
primTy _ PrimFst = return $  TypSch ["x", "y"] $ (TyProd (TyVar "x") (TyVar "y")) `TyFn` (TyVar "x")
primTy _ PrimSnd = return $  TypSch ["x", "y"] $ (TyProd (TyVar "x") (TyVar "y")) `TyFn` (TyVar "y")
primTy _ PrimNil = return $  TypSch ["t"] $ TyList (TyVar "t")
primTy _ PrimCons = return $ TypSch ["t"] $ TyFn (TyVar "t") (TyFn (TyList (TyVar "t")) (TyList (TyVar "t")))
primTy _ PrimTT = return $ TypSch [] TyUnit
primTy _ PrimFF = return $ TypSch ["t"] $ TyFn TyVoid (TyVar "t")
primTy _ PrimInl = return $ TypSch ["x", "y"] $ (TyVar "x") `TyFn` (TyProd (TyVar "x") (TyVar "y"))
primTy _ PrimInr = return $ TypSch ["x", "y"] $ (TyVar "y") `TyFn` (TyProd (TyVar "x") (TyVar "y"))
primTy _ PrimPair = return $ TypSch ["xx", "yy"] $ (TyFn (TyVar "xx") (TyFn (TyVar "yy") (TyProd (TyVar "xx") (TyVar "yy"))))
primTy _ PrimIf = return $ TypSch [] $ natType `TyFn` (natType `TyFn` (natType `TyFn` natType))
primTy _ FoldList = return $ TypSch ["a", "b"] $ p `TyFn` ((TyVar "b") `TyFn` ((TyList $ TyVar "a") `TyFn` (TyVar "b")))
 where p = TyVar "b" `TyFn` (TyVar "a" `TyFn` TyVar "b")
primTy _ PrimCase = return $ TypSch ["x", "y", "z"] $ (TySum (TyVar "x") (TyVar "y")) `TyFn` (l `TyFn` (r `TyFn` (TyVar "z")))
 where l = (TyVar "x") `TyFn` (TyVar "z")
       r = (TyVar "y") `TyFn` (TyVar "z")

-----------------------------
-- Substitution

type MTySubst = M.Map Var MTy
type FTySubst = M.Map Var FTy
type FExprSubst = M.Map Var FExpr

idMTySubst :: MTySubst
idMTySubst = M.empty

composeMTySubst :: MTySubst -> MTySubst -> MTySubst
composeMTySubst f g = M.union addExtra $ fmap (substMTy f) g
  where
    addExtra = M.filterWithKey (\v _ -> Y.isNothing $ M.lookup v g) f

substMTy :: MTySubst -> MTy -> MTy
substMTy f t = case t of
  TyLit lt -> TyLit lt
  TyVariant ts -> TyVariant $ map (substMTy f) ts
  TyTuple ts -> TyTuple $ map (substMTy f) ts
  TyUnit -> TyUnit
  TyVoid -> TyVoid
  TyList t -> TyList $ substMTy f t
  TyFn t1 t2 -> TyFn (substMTy f t1) (substMTy f t2)
  TyProd t1 t2 -> TyProd  (substMTy f t1) (substMTy f t2)
  TySum t1 t2 -> TySum  (substMTy f t1) (substMTy f t2)
  TyVar v -> case M.lookup v f of
    Nothing -> TyVar v
    Just y -> y
  TyCon v ts -> TyCon v $ map (substMTy f) ts

mTySubstFTy :: MTySubst -> FTy -> FTy
mTySubstFTy f t = case t of
  FTyLit lt -> FTyLit lt
  FTyVariant ts -> FTyVariant $ map (mTySubstFTy f) ts
  FTyTuple ts -> FTyTuple $ map (mTySubstFTy f) ts
  FTyUnit -> FTyUnit
  FTyVoid -> FTyVoid
  FTyList t -> FTyList $ mTySubstFTy f t
  FTyFn t1 t2 -> FTyFn (mTySubstFTy f t1) (mTySubstFTy f t2)
  FTyProd t1 t2 -> FTyProd  (mTySubstFTy f t1) (mTySubstFTy f t2)
  FTySum t1 t2 -> FTySum  (mTySubstFTy f t1) (mTySubstFTy f t2)
  FTyVar v -> case M.lookup v f of
    Nothing -> FTyVar v
    Just y -> mTyToFTy y
  FTyForall vs t -> FTyForall vs $ mTySubstFTy phi2 t
    where
      phi2 = M.filterWithKey (\ v _ -> not (elem v vs)) f
  FTyCon v ts -> FTyCon v $ map (mTySubstFTy f) ts

substTypeScheme :: MTySubst -> TypSch -> TypSch
substTypeScheme f (TypSch vs t) = TypSch vs $ substMTy f2 t
  where
    f2 = M.filterWithKey (\v _ -> not $ elem v vs) f

substContext :: MTySubst -> Ctx -> Ctx
substContext phi = fmap $ substTypeScheme phi

mTySubstFExpr :: MTySubst -> FExpr -> FExpr
mTySubstFExpr phi e = case e of
 FInj i js e -> FInj i (map (mTySubstFTy phi) js) $ mTySubstFExpr phi e
 FProj i e -> FProj i $ mTySubstFExpr phi e
 FTuple es -> FTuple $ map (mTySubstFExpr phi) es
 FCase e t es -> FCase (mTySubstFExpr phi e) (mTySubstFTy phi t) $ map (mTySubstFExpr phi) es
 FConst p -> FConst p
 FVar p -> FVar p
 FApp p q -> FApp (mTySubstFExpr phi p) (mTySubstFExpr phi q)
 FAbs p t q -> FAbs p (mTySubstFTy phi t) (mTySubstFExpr phi q)
 FTyApp p q -> FTyApp (mTySubstFExpr phi p) (map (mTySubstFTy phi) q)
 FTyAbs vs p -> FTyAbs vs (mTySubstFExpr phi2 p)
   where
     phi2 = M.filterWithKey (\v _ -> not (elem v vs)) phi
 FLetrec vs p -> FLetrec (map (\(k,t,v)->(k, mTySubstFTy phi t, mTySubstFExpr phi v)) vs) (mTySubstFExpr phi p)

substFExpr :: FExprSubst -> FExpr -> FExpr
substFExpr phi (FTuple es) = FTuple $ map (substFExpr phi) es
substFExpr phi (FConst c) = FConst c
substFExpr phi (FVar v') = case M.lookup v' phi of
                         Just y -> y
                         Nothing -> FVar v'
substFExpr phi (FApp a b) = FApp (substFExpr phi a) (substFExpr phi b)
substFExpr phi (FAbs v' a b) = FAbs v' a $ substFExpr phi' b
  where
    phi' = M.filterWithKey (\k _ -> k /= v') phi
substFExpr phi (FTyApp a ts) = FTyApp (substFExpr phi a) ts
substFExpr phi (FTyAbs vs a) = FTyAbs vs $ substFExpr phi a
substFExpr phi (FLetrec es e) = FLetrec (map (\(k,t,f)->(k,t,substFExpr phi' f)) es) (substFExpr phi' e)
  where
    phi' = M.filterWithKey (\k _ -> not (elem k ns)) phi
    (ns,ts,es') = unzip3 es

substFTy :: FTySubst -> FTy -> FTy
substFTy f (FTyLit lt) = FTyLit lt
substFTy f (FTyTuple ts) = FTyTuple $ (map $ substFTy f) ts
substFTy f (FTyVariant ts) = FTyVariant $ (map $ substFTy f) ts
substFTy f FTyUnit = FTyUnit
substFTy f FTyVoid = FTyVoid
substFTy f (FTyList t) = FTyList $ substFTy f t
substFTy f (FTyFn t1 t2) = FTyFn (substFTy f t1) (substFTy f t2)
substFTy f (FTyProd t1 t2) = FTyProd  (substFTy f t1) (substFTy f t2)
substFTy f (FTySum t1 t2) = FTySum  (substFTy f t1) (substFTy f t2)
substFTy f (FTyVar v) = case M.lookup v f of
  Nothing -> FTyVar v
  Just y -> y
substFTy f (FTyForall vs t) = FTyForall vs $ substFTy f2 t
  where
    f2 = M.filterWithKey (\v _ -> not (elem v vs)) f
substFTy f (FTyCon v ts) = FTyCon v $ map (substFTy f) ts

------------------------------------
-- Type checking for F

open :: [Var] -> [FTy] -> FTy -> Either String FTy
open vs ts e | length vs == length ts = return $ substFTy (M.fromList $ zip vs ts) e
             | otherwise = throwError "Cannot open"

typeOf :: ADTs -> [Var] -> M.Map Var FTy -> FExpr -> Either String FTy
typeOf adts tvs g (FCase e r es) = do { ts <- mapM (typeOf adts tvs g) es
                                      ; t <- typeOf adts tvs g e
                                      ; case t of
                                         FTyVariant ts' -> if length ts == length ts'
                                                           then do { mapM f $ zip ts ts'
                                                                   ; return r }
                                                           else throwError $ "bad number of cases "
                                         z -> throwError $ show z ++ " is not a variant"}
 where f ((FTyFn a b), exp) | a == exp && b == r = return ()
typeOf adts tvs g (FInj i ts e) = do { t <- typeOf adts tvs g e
                                     ; if i >= length ts
                                       then throwError $ "bad inj index " ++ show i ++ " on " ++ show e
                                       else if ts !! i == t
                                            then return $ FTyVariant ts
                                            else throwError $ "injection type mismatch " ++ show (FInj i ts e) ++ ", " ++ show (ts !! i) ++ " <> " ++ show t }
typeOf adts tvs g (FProj i e) = do { t <- typeOf adts tvs g e
                                   ; case t of
                                      FTyTuple ts -> if i >= length ts
                                                     then throwError $ "bad proj index " ++ show i ++ " on " ++ show e
                                                     else return $ ts !! i
                                      z -> throwError $ show z ++ " is not a tuple type" }
typeOf adts tvs g (FTuple es) = do { ts <- mapM (typeOf adts tvs g) es
                                   ; return $ FTyTuple ts }
typeOf adts tvs g (FVar x) = case M.lookup x g of
  Nothing -> throwError $ "unbound var: " ++ x
  Just y -> return y
typeOf adts tvs g (FConst p) = do { t <- primTy adts p ; return $ tyToFTy t }
typeOf adts tvs g (FApp a b) = do { t1 <- typeOf adts tvs g a
                                  ; t2 <- typeOf adts tvs g b
                                  ; case t1 of
                                      FTyFn p q -> if p == t2
                                                   then return q
                                                   else throwError $ "In " ++ (show $ FApp a b) ++ " expected " ++ show p ++ " given " ++ show t2
                                      v -> throwError $ "In " ++ show (FApp a b) ++ " not a fn type: " ++ show v }
typeOf adts tvs g (FAbs x t e) = do { t1 <- typeOf adts tvs (M.insert  x t g) e
                                    ; return $ t `FTyFn` t1 }
typeOf adts tvs g (FTyAbs vs e) = do { t1 <- typeOf adts (vs++tvs) g e
                                     ; return $ FTyForall vs t1 }
typeOf adts tvs g (FTyApp e ts) = do { t1 <- typeOf adts tvs g e
                                     ; case t1 of
                                        FTyForall vs t -> open vs ts t
                                        v -> throwError $ "not a forall type: " ++ show v }
typeOf adts tvs g (FLetrec es e) = do { let g' = M.fromList $ fmap (\(k,t,e) -> (k,t)) es
                                      ; est <- mapM (\(_,_,v)->typeOf adts tvs (M.union g' g) v) es
                                      ; if est == M.elems g'
                                        then typeOf adts tvs g' e
                                        else throwError $ "Disagree: " ++ show est ++ " and " ++ (show $ M.elems g') }


-----------------------------
-- Unification

-- Find the MGU of two types
unify :: MTy -> MTy -> E MTySubst
unify (TyLit lt1) (TyLit lt2) = if lt1 == lt2
  then return M.empty
  else throwError $ "Cannot unify literal types " ++ show lt1 ++ " and " ++ show lt2
unify (TyVariant ts1) (TyVariant ts2) | length ts1 == length ts2 = unifyMany ts1 ts2
                                    | otherwise = throwError $ "cannot unify " ++ show ts1 ++ " with " ++ show ts2
unify (TyTuple ts1) (TyTuple ts2) | length ts1 == length ts2 = unifyMany ts1 ts2
                                | otherwise = throwError $ "cannot unify " ++ show ts1 ++ " with " ++ show ts2
unify (TyList a) (TyList b) = unify a b
unify TyUnit TyUnit = return M.empty
unify TyVoid TyVoid = return M.empty
unify (TyCon a as) (TyCon b bs) | a == b && length as == length bs = unifyMany as bs
                              | otherwise = throwError $ "cannot unify " ++ show a ++ " with " ++ show b
unify (TyProd a b) (TyProd a' b') = do { s <- unify a a' ; s' <- unify (substMTy s b) (substMTy s b'); return $ composeMTySubst s' s }
unify (TySum  a b) (TySum  a' b') = do { s <- unify a a' ; s' <- unify (substMTy s b) (substMTy s b'); return $ composeMTySubst s' s }
unify (TyFn   a b) (TyFn   a' b') = do { s <- unify a a' ; s' <- unify (substMTy s b) (substMTy s b'); return $ composeMTySubst s' s }
unify (TyVar a) (TyVar b) | a == b = return M.empty
unify (TyVar a) b = do { occurs a b; return $ M.singleton a b }
unify a (TyVar b) = unify (TyVar b) a
unify a b = throwError $ "cannot unify " ++ show a ++ " with " ++ show b

unifyMany :: [MTy] -> [MTy] -> E MTySubst
unifyMany [] [] = return idMTySubst
unifyMany (a:as) (b:bs) = do { f <- unify a b; s <- unifyMany (map (substMTy f) as) (map (substMTy f) bs); return $ composeMTySubst s f }

occurs :: Var -> MTy -> E ()
occurs v (TyLit _) = return ()
occurs v (TyCon _ ts) = mapM_ (occurs v) ts
occurs v (TyTuple ts) = mapM_ (occurs v) ts
occurs v (TyVariant ts) = mapM_ (occurs v) ts
occurs v (TyList l) = occurs v l
occurs v TyUnit = return ()
occurs v TyVoid = return ()
occurs v (TyFn   a b) = do { occurs v a; occurs v b }
occurs v (TyProd a b) = do { occurs v a; occurs v b }
occurs v (TySum  a b) = do { occurs v a; occurs v b }
occurs v (TyVar v') | v == v' = throwError $ "occurs check failed"
                    | otherwise = return ()

-----------------------------
-- Algorithm W

type E = ExceptT String (State ([String],Integer))
type M a = E (MTySubst, a)

log0 :: Int -> String -> E ()
log0 i x = do { (l, s) <- get; put ((x'++x):l, s); }
 where x' = foldl (\p q -> "\t" ++ p) "" [0..i]

fresh :: E MTy
fresh = do { (l,s) <- get; put (l, s + 1); return $ TyVar $ "v" ++ show s }

instantiate :: TypSch -> E (MTy,[MTy])
instantiate (TypSch vs ty) = do { vs' <- mapM (\_->fresh) vs; return $ (substMTy (M.fromList $ zip vs vs') ty,  vs') }

generalize :: Ctx -> MTy -> TypSch
generalize g t = TypSch vars t
  where
    vars = nub $ filter isUnbound $ variablesInType t
    isUnbound v = not $ elem v $ variablesInContext g

fTyApp x [] = x
fTyApp x y = FTyApp x y

fTyAbs [] x = x
fTyAbs x y = FTyAbs x y

replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)

checkAgainstF :: ADTs -> Ctx -> MTy -> FExpr -> E ()
checkAgainstF adts g t e = case (typeOf adts [] g' e) of
                              Left err -> throwError  $ "\t" ++  "err: " ++ err
                              Right tt -> if tt == mTyToFTy t
                                          then return ()
                                          else throwError $ show g ++ "|- " ++ show e ++ ": " ++ show tt ++ " != " ++ show t
  where g' = fmap tyToFTy g

-- | Infer the type of an expression using Algorithm W
infer :: Int -> ADTs -> Ctx -> Expr -> M (MTy, FExpr)
infer level adts g (ExprAbs x e) = do
    vdom <- fresh
    (sbody, (tbody, ebody)) <- infer (level+1) adts (M.insert x (TypSch [] vdom) g) e
    let rExpr = FAbs x (mTyToFTy $ substMTy sbody vdom) ebody
        rType = TyFn (substMTy sbody vdom) tbody
    log0 level $ "ABS " ++ show (substContext sbody g) ++ "|- \\" ++ x ++ ". " ++ show rExpr ++ " : " ++ show rType
    checkAgainstF adts (substContext sbody g) rType rExpr
    return (sbody, (rType, rExpr))
infer l adts g (ExprApp e0 e1) = do
    (s0, (t0, a)) <- infer (l+1) adts g e0
    (s1, (t1, b)) <- infer (l+1) adts (substContext s0 g) e1
    t' <- fresh
    s2 <-  (substMTy s1 t0) `unify` (t1 `TyFn` t')
    let rExpr = FApp (mTySubstFExpr (composeMTySubst s2 s1) a) (mTySubstFExpr s2 b)
        rType = substMTy s2 t'
        rSubst = composeMTySubst s2 $ composeMTySubst s1 s0;
    log0 l $ "APP " ++ show (substContext rSubst g) ++ "|- " ++ show rExpr ++ " : " ++  show rType
    checkAgainstF adts (substContext rSubst g) rType rExpr
    return (rSubst, (rType, rExpr))
infer level adts g (ExprCase e es') = do
    t <- fresh
    bs' <- mapM (\_->fresh) es'
    let bs = map (\k-> TyFn k t) bs'
    (phi , (ts, es)) <- inferMany (level+1) adts g es'
    m1 <- unifyMany bs ts
    (phi', (t', e')) <- infer  (level+1) adts (substContext (composeMTySubst m1 phi) g) e
    m2 <- unify (substMTy (composeMTySubst m1 phi) t') (substMTy (composeMTySubst m1 $ composeMTySubst phi' phi) $ TyVariant bs')
    let rExpr = FCase (mTySubstFExpr m2 e') (mTyToFTy rType) (map (mTySubstFExpr $ composeMTySubst m2 $ composeMTySubst phi' m1) es)
        rType = substMTy rSubst t
        rSubst = composeMTySubst m2 $ composeMTySubst phi' $ composeMTySubst m1 phi
    checkAgainstF adts (substContext rSubst g) rType rExpr
    return (rSubst, (rType, rExpr))
infer level adts g (ExprConst p) = case primTy adts p of
    Left er -> throwError er
    Right t -> do
      (rType, vs) <- instantiate t
      let rExpr = fTyApp (FConst p) $ map mTyToFTy vs
      log0 level $ "CONST " ++ show g ++ "|- " ++  show rExpr ++ " : " ++ show rType
      checkAgainstF adts g rType rExpr
      return (idMTySubst, (rType, rExpr))
infer level adts g (ExprInj i j e) = do
    (s0, (t0, a)) <- infer (level+1) adts g e
    t'' <- mapM (\_->fresh) [1..j]
    let t' = replace t'' (i, t0)
    let rExpr = FInj i (map (mTyToFTy) t') a
        rType = TyVariant t'
        rSubst = s0
    log0 level $ "INJ " ++ show (substContext rSubst g) ++ "|- " ++ (show rExpr) ++ " : " ++  show rType
    checkAgainstF adts (substContext rSubst g) rType rExpr
    return (rSubst, (rType, rExpr))
infer level adts g (ExprLetrec b0 env0) = do
    -- Fresh type variables; one per binding
    bvars <- CM.replicateM (length b0) fresh

    -- Extend the environment with temporary types for each binding
    let g2 = M.union (M.fromList $ zip bnames $ fmap (TypSch []) bvars) g
    -- Infer the types of the bindings
    (s1, (tb, eb1)) <- inferMany (level+1) adts g2 eb0
    sb <- unifyMany (fmap (substMTy s1) bvars) tb

    -- Extend the environment again with the inferred types. TODO: this actually restricts the environment to the bindings. Add test cases for nested let, e.g. let x = 42 in let y = 37 in x+y
    let g3 = M.fromList $ zip bnames $ fmap (generalize g . substMTy sb) tb
    -- Infer the type of the let environment
    (senv, (tenv, env1)) <- infer (level+1) adts g3 env0
    let s2 = composeMTySubst senv $ composeMTySubst sb s1
        eb2 = fmap (mTySubstFExpr (composeMTySubst senv sb)) eb1

    let b3t = zip bnames $ fmap (generalize g . substMTy sb) tb
    let s3 = M.fromList $ fmap (\(x, ts) -> (x, (fTyApp (FVar x) $ fmap FTyVar $ typSchVariables ts))) b3t :: FExprSubst
        b3 = L.zipWith (\(x, ts) e ->
               (x,
                mTySubstFTy senv $ tyToFTy ts,
                mTySubstFExpr (composeMTySubst sb senv) $ fTyAbs (typSchVariables ts) $ substFExpr s3 e)) b3t eb2 :: [(Var, FTy, FExpr)]
    let let1 = FLetrec b3 env1

    log0 level $ "LETREC " ++ show (substContext s2 g) ++ "|- " ++ show let1 ++ " : " ++ show tenv
    checkAgainstF adts (substContext s2 g) tenv let1
    return (s2, (tenv, let1))
  where
    bnames = fmap fst b0
    eb0 = fmap snd b0
infer level adts g (ExprProj i j e) = do
    (s0, (t0, a)) <- infer (level+1) adts g e
    t' <- mapM (\_->fresh) [1..j]
    s2 <-  t0 `unify` (TyTuple t')
    let rExpr = FProj i (mTySubstFExpr s2 a)
        rType = substMTy s2 (t' !! i)
        rSubst = composeMTySubst s2 s0
    log0 level $ "PROJ " ++ show (substContext rSubst g) ++ "|- " ++ (show rExpr) ++ " : " ++  show rType
    checkAgainstF adts (substContext rSubst g) rType rExpr
    return (rSubst, (rType, rExpr))
infer level adts g (ExprTuple es') = do
    (phi, (ts, es)) <- inferMany (level+1) adts g es'
    let rExpr = FTuple es
        rType = TyTuple ts
        rSubst = phi
    log0 level $ "TUPLE " ++ show (substContext rSubst g) ++ "|- " ++ (show rExpr) ++ " : " ++  show rType
    checkAgainstF adts (substContext rSubst g) rType rExpr
    return (rSubst, (rType, rExpr))
infer level adts g (ExprVar x) = case M.lookup x g of
    Nothing -> throwError $ "Unknown var: " ++ (show x)
    Just s -> do
      (rType, vs) <- instantiate s
      let rExpr = fTyApp (FVar x) $ map mTyToFTy vs
      log0 level $ "VAR " ++ show g ++ "|- " ++ show rExpr ++ " : " ++ show rType
      checkAgainstF adts g rType rExpr
      return (idMTySubst, (rType, rExpr))

inferMany :: Int -> ADTs -> Ctx -> [Expr] -> M ([MTy], [FExpr])
inferMany level adts g es = case es of
  [] -> return (idMTySubst, ([], []))
  (e:tl) -> do
    (s1, (t1, e1)) <- infer level adts g e
    (s2, (t2, e2)) <- inferMany level adts (substContext s1 g) tl
    return (composeMTySubst s2 s1, ((substMTy s2 t1):t2, (mTySubstFExpr s2 e1):e2))

----------------------------------------
-- Main

data TestCase = TestCase String Expr

theAdts = theAdtsJosh ++ [("List", ["t"], [("Nil", []), ("Cons", [TyVar "t", TyCon "List" [TyVar "t"]])])]

theAdtsJosh = [("LatLon1", [], [("MkLatLon1", [natType, natType])]),
               ("LatLon2", [], [("MkLatLon2", [natType, natType])])]
{--  data LatLon1 = MkLatOn1 Nat Nat where lat1 (MkLatLon1 a) = a and lon1 (MkLatLon1 b) = b and also data LatLon2 = MkLatOn2 Nat Nat where lat2 (MkLatLon2 a ) = a and lon2 (MkLatLon2  b) = --}

testOne (TestCase name t) = do { putStrLn $ "[" ++ name ++ "]"
               ; putStrLn $ "Untyped input: "
               ; putStrLn $ "\t" ++  show t
               ; let out = runState (runExceptT (infer 0 theAdts M.empty t)) ([],0)
               ; case fst out of
                   Left  e -> do { putStrLn $ "\t" ++ "err: " ++ e
                                 ; putStrLn $ "\nLog:"
                                 ; mapM_ (putStrLn) $ reverse $ fst $ snd out }
                   Right (s, (ty, f)) -> do { putStrLn $ "\nType inferred by Hindley-Milner: "
                                            ; putStrLn $ "\t" ++ show ty
                                            ; putStrLn "\nSystem F translation: "
                                            ; putStrLn $ "\t" ++ show f
                                            -- ; putStrLn "\nLog: "
                                            -- ; mapM_ (putStrLn) $ reverse $ fst $ snd out
                                            }
               ; putStrLn ""
               ; putStrLn "------------------------"
               ; putStrLn ""  }

letrec' x e f = ExprLetrec [(x,e)] f

x @@ y = ExprApp x y
zero = nat 0


-- Run tests ---------------------------

main = mapM testOne tests

tests = adtTests ++ tupleTests ++ variantTests ++ letTests

---

adtTests = [testAdt1, testAdt2, testAdt3]

testAdt1 = TestCase "testAdt1" $ ExprLetrec [("lat1", lat1)] body
 where body = ExprApp (ExprApp (ExprConst $ PrimCon "MkLatLon1") (nat 0)) (nat 1)
       lat1 = ExprAbs "z" $ ExprApp (ExprApp (ExprConst $ PrimFold "LatLon1") (ExprVar "z")) $ ExprAbs "p" $ ExprAbs "q" $ (ExprVar "p")

testAdt2 = TestCase "testAdt2" $ ExprLetrec [("lat1", lat1)] $ ExprApp (ExprVar "lat1") body
 where body = ExprApp (ExprApp (ExprConst $ PrimCon "MkLatLon1") (nat 0)) (nat 1)
       lat1 = ExprAbs "z" $ ExprApp (ExprApp (ExprConst $ PrimFold "LatLon1") (ExprVar "z")) $ ExprAbs "p" $ ExprAbs "q" $ (ExprVar "p")

testAdt3 = TestCase "testAdt3" $ ExprAbs "z" $ ExprConst (PrimFold "List")
  @@ ExprVar "z"
  @@ ExprConst (PrimCon "Nil")
  @@ ExprConst (PrimCon "Cons")

---

tupleTests = [testTuple1, testTuple2]

testTuple1 = TestCase "testTuple1" $ ExprLetrec [("t", t)] $ ExprTuple [ExprProj 1 2 $ ExprVar "t", ExprProj 0 2 $ ExprVar "t"]
 where f = str "alice"
       g = nat 0
       t = ExprTuple [f, g]

testTuple2 = TestCase "testTuple2" $ ExprAbs "z" $ ExprTuple [(ExprProj 0 2 $ ExprVar "z"), (ExprProj 1 2 $ ExprVar "z")]

---

variantTests = [testVariant1, testVariant2]

testVariant1 = TestCase "testVariant1" $ ExprAbs "z" $ ExprCase (ExprVar "z") [f,g]
 where f = ExprAbs "x" {-- $ ExprInj 0 2 --} $ ExprVar "x"
       g = ExprAbs "y" {-- $ ExprInj 1 2 --} $ ExprVar "y"

testVariant2 = TestCase "testVariant2" $ ExprAbs "z" $ ExprCase (ExprVar "z") [f,g]
 where f = ExprAbs "x" $ ExprInj 0 2 $ ExprVar "x"
       g = ExprAbs "y" $ ExprInj 1 2 $ ExprVar "y"

---

letTests = [testLet1, testLet2, testLet3, testLet4, testLet5, testLet6, testLet7, testLet8, testLet9, testLet10, testLet11, testLet12, testLet13]

testLet1 = TestCase "testLet1" $ ExprLetrec [("f", f), ("g", g)] b
 where b = ExprTuple [(ExprVar "f"), (ExprVar "g")]
       f = ExprAbs "x" $ ExprAbs "y" $ ExprApp (ExprApp (ExprVar "g") (nat 0)) (ExprVar "x")
       g = ExprAbs "u" $ ExprAbs "v" $ ExprApp (ExprApp (ExprVar "f") (ExprVar "v")) (nat 0)

testLet2 = TestCase "testLet2" $ letrec' "f" i b
 where i = ExprAbs "x" (ExprVar "x")
       b = ExprVar "f"

testLet3 = TestCase "testLet3" $ letrec' "f" ( (ExprAbs "x" (ExprVar "x"))) $ ExprApp (ExprVar "f")  (nat 0)
 where sng0 = ExprApp (ExprVar "sng") (nat 0)
       sngAlice = ExprApp (ExprVar "sng") (str "alice")
       body = (ExprVar "sng")

testLet4 =  TestCase "testLet4" $ letrec' "f" (ExprApp (ExprAbs "x" (ExprVar "x")) (nat 0)) (ExprVar "f")
 where sng0 = ExprApp (ExprVar "sng") (nat 0)
       sngAlice = ExprApp (ExprVar "sng") (str "alice")

testLet5 = TestCase "testLet5" $ letrec' "sng" (ExprAbs "x" (ExprApp (ExprApp (ExprConst PrimCons) (ExprVar "x")) (ExprConst PrimNil))) body
  where
    body = (ExprVar "sng")

testLet6 = TestCase "testLet6" $ letrec' "sng" (ExprAbs "x" (ExprApp (ExprApp (ExprConst PrimCons) (ExprVar "x")) (ExprConst PrimNil))) body
 where sng0 = ExprApp (ExprVar "sng") (nat 0)
       sngAlice = ExprApp (ExprVar "sng") (str "alice")
       body = ExprApp (ExprApp (ExprConst PrimPair) sng0) sngAlice

testLet7 = TestCase "testLet7" $ letrec' "+" (ExprAbs "x" $ ExprAbs "y" $ recCall) twoPlusOne
 where
   recCall = ExprApp constSucc $ ExprApp (ExprApp (ExprVar "+") (ExprApp constPred (ExprVar "x"))) (ExprVar "y")
   ifz x y z = ExprApp (ExprApp (ExprApp (ExprConst PrimIf) x) y) z
   twoPlusOne = ExprApp (ExprApp (ExprVar "+") two) one
   two = ExprApp constSucc one
   one = ExprApp constSucc (nat 0)

testLet8 = TestCase "testLet8" $ letrec' "+" (ExprAbs "x" $ ExprAbs "y" $ recCall) $ twoPlusOne
 where
   recCall = ExprApp constSucc $ ExprApp (ExprApp (ExprVar "+") (ExprApp constPred (ExprVar "x"))) ( (ExprVar "y"))
   ifz x y z = ExprApp (ExprApp (ExprApp (ExprConst PrimIf) x) y) z
   twoPlusOne = ExprApp (ExprApp (ExprVar "+") two) one
   two = ExprApp constSucc one
   one = ExprApp constSucc (nat 0)

testLet9 = TestCase "testLet9" $ letrec' "f" f x
 where x =  (ExprVar "f")
       f = ExprAbs "x" $ ExprAbs "y" $ ExprApp (ExprApp (ExprVar "f") (nat 0)) (ExprVar "x")

testLet10 = TestCase "testLet10" $ ExprLetrec [("f", f), ("g", g)] x
 where x =  ExprApp (ExprApp (ExprConst PrimPair) (ExprVar "f")) (ExprVar "g")
       f = ExprAbs "x" $ ExprAbs "y" $ ExprApp (ExprApp (ExprVar "f") (nat 0)) (ExprVar "x")
       g = ExprAbs "xx" $ ExprAbs "yy" $ ExprApp (ExprApp (ExprVar "g") (nat 0)) (ExprVar "xx")

testLet11 = TestCase "testLet11" $ ExprLetrec [("f", f), ("g", g)] b
 where b = ExprApp (ExprApp (ExprConst PrimPair) (ExprVar "f")) (ExprVar "g")
       f = ExprAbs "x" $ ExprAbs "y" $ ExprApp (ExprApp (ExprVar "g") (nat 0)) (ExprVar "x")
       g = ExprAbs "u" $ ExprAbs "v" $ ExprApp (ExprApp (ExprVar "f") (ExprVar "v")) (nat 0)

testLet12 = TestCase "testLet12" $ ExprLetrec [("f", f), ("g", g)] b
 where b = ExprApp (ExprApp (ExprConst PrimPair) (ExprVar "f")) (ExprVar "g")
       f = ExprAbs "x" $ ExprAbs "y" $ ExprApp (ExprApp (ExprVar "g") (nat 0)) (nat 0)
       g = ExprAbs "u" $ ExprAbs "v" $ ExprApp (ExprApp (ExprVar "f") (ExprVar "v")) (nat 0)

testLet13 = TestCase "testLet13" $ ExprLetrec [("f", f), ("g", g)] b
 where b = ExprApp (ExprApp (ExprConst PrimPair) (ExprVar "f")) (ExprVar "g")
       f = ExprAbs "x" $ ExprAbs "y" $ ExprApp (ExprApp (ExprVar "g") (nat 0)) (ExprVar "x")
       g = ExprAbs "u" $ ExprAbs "v" $ ExprApp (ExprApp (ExprVar "f") (nat 0)) (nat 0)

testLet14 = TestCase "testLet14" $ ExprLetrec [("singleton", singleton), ("f", f), ("g", g)] $ ExprVar "f"
  where
    singleton = ExprAbs "x" $ ExprConst PrimCons @@ ExprVar "x" @@ ExprConst PrimNil
    f = ExprAbs "x" $ ExprAbs "y" $ ExprConst PrimCons
      @@ (ExprConst PrimPair
        @@ (ExprVar "singleton" @@ ExprVar "x")
        @@ (ExprVar "singleton" @@ ExprVar "y"))
      @@ (ExprVar "g" @@ ExprVar "x" @@ ExprVar "y")
    g = ExprAbs "x" $ ExprAbs "y" $ ExprVar "f" @@ zero @@ ExprVar "y"

testLet14_haskell = f
  where
    sng = \x -> [x]
    f = \x y -> (sng x, sng y): []
    g = \x y -> f 0 y

testLet15 = TestCase "testLet15" $ ExprLetrec [("singleton", singleton)] $
 ExprLetrec [("f", f), ("g", g)] $ ExprVar "f"
  where
    singleton = ExprAbs "x" $ ExprConst PrimCons @@ ExprVar "x" @@ ExprConst PrimNil
    f = ExprAbs "x" $ ExprAbs "y" $ ExprConst PrimCons
      @@ (ExprConst PrimPair
        @@ (ExprVar "singleton" @@ ExprVar "x")
        @@ (ExprVar "singleton" @@ ExprVar "y"))
      @@ (ExprVar "g" @@ ExprVar "x" @@ ExprVar "y")
    g = ExprAbs "x" $ ExprAbs "y" $ ExprVar "f" @@ zero @@ ExprVar "y"
