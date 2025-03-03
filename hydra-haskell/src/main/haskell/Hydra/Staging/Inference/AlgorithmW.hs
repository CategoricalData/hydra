{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- Implementation of Hindley Milner algorithm W
-- to system F translation by Ryan Wisnesky, with extensions for Hydra by Joshua Shinavier
-- License: Apache 2.0 https://www.apache.org/licenses/LICENSE-2.0

module Hydra.Staging.Inference.AlgorithmW where

import Hydra.Minimal

import Prelude
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Debug.Trace


natType = TyLit $ LiteralTypeInteger IntegerTypeInt32
constNeg = ExprConst $ PrimTyped $ TypedPrimitive (Name "hydra.lib.math.neg") $ Forall [] $ TyFn natType natType
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

data MTy = TyVar Var
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

data TypSch = Forall [Var] MTy
 deriving Eq

instance Show TypSch where
  show (Forall [] t) = show t
  show (Forall x t) = "forall " ++ d ++ show t
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
tyToFTy (Forall [] t) = mTyToFTy t
tyToFTy (Forall vs t) = FTyForall vs (mTyToFTy t)

--------------------
-- Contexts

type Ctx = [(Var, TypSch)]

class Vars a where
  vars :: a -> [Var]

instance Vars Ctx where
 vars [] = []
 vars ((v,t):l) = vars t ++ vars l

instance Vars TypSch where
 vars (Forall vs t) = filter (\v -> not $ elem v vs) (vars t)

instance Vars MTy where
 vars (TyVar v) = [v]
 vars (TyLit _) = []
 vars (TyList t) = vars t
 vars (TyFn t1 t2) = vars t1 ++ vars t2
 vars TyUnit = []
 vars TyVoid = []
 vars (TyProd t1 t2) = vars t1 ++ vars t2
 vars (TySum t1 t2) = vars t1 ++ vars t2
 vars (TyTuple []) = []
 vars (TyTuple (a:b)) = vars a ++ vars (TyTuple b)
 vars (TyVariant []) = []
 vars (TyVariant (a:b)) = vars a ++ vars (TyVariant b)
 vars (TyCon _ x) = vars $ TyVariant x --cheat

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
primTy _ (PrimLiteral l) = Right $ Forall [] $ TyLit $ literalType l
primTy _ (PrimTyped (TypedPrimitive _ forAll)) = Right forAll
primTy [] (PrimCon n) = throwError $ n ++ " not found "
primTy ((a,t,[]):tl) (PrimCon n) = primTy tl $ PrimCon n
primTy ((a,t,(c,ts):cs):tl) (PrimCon n) | c == n = return $ Forall t (ts' ts $ TyCon a $ map TyVar t)
                                    | otherwise = primTy ((a,t,cs):tl) $ PrimCon n
 where ts' [] x = x
       ts' (p:q) x = TyFn p $ ts' q x
primTy [] (PrimFold n) = throwError $ n ++ " not found "
primTy ((a,t,cs):tl) (PrimFold n) | a == n = return $ Forall ("r":t) $ elimTy
                              | otherwise = primTy tl $ PrimFold n
 where elimTy = TyFn (TyCon a $ map TyVar t) $ replaceTCon (TyCon a $ map TyVar t) (TyVar "r") (ts' cs)
       ts' [] = TyVar "r"
       ts' ((_,ts):q)  = TyFn (ts'' ts) $ ts' q
       ts'' [] = TyCon a $ map TyVar t
       ts'' (a:b) = TyFn a $ ts'' b
primTy _ PrimFst = return $  Forall ["x", "y"] $ (TyProd (TyVar "x") (TyVar "y")) `TyFn` (TyVar "x")
primTy _ PrimSnd = return $  Forall ["x", "y"] $ (TyProd (TyVar "x") (TyVar "y")) `TyFn` (TyVar "y")
primTy _ PrimNil = return $  Forall ["t"] $ TyList (TyVar "t")
primTy _ PrimCons = return $ Forall ["t"] $ TyFn (TyVar "t") (TyFn (TyList (TyVar "t")) (TyList (TyVar "t")))
primTy _ PrimTT = return $ Forall [] TyUnit
primTy _ PrimFF = return $ Forall ["t"] $ TyFn TyVoid (TyVar "t")
primTy _ PrimInl = return $ Forall ["x", "y"] $ (TyVar "x") `TyFn` (TyProd (TyVar "x") (TyVar "y"))
primTy _ PrimInr = return $ Forall ["x", "y"] $ (TyVar "y") `TyFn` (TyProd (TyVar "x") (TyVar "y"))
primTy _ PrimPair = return $ Forall ["xx", "yy"] $ (TyFn (TyVar "xx") (TyFn (TyVar "yy") (TyProd (TyVar "xx") (TyVar "yy"))))
primTy _ PrimIf = return $ Forall [] $ natType `TyFn` (natType `TyFn` (natType `TyFn` natType))
primTy _ FoldList = return $ Forall ["a", "b"] $ p `TyFn` ((TyVar "b") `TyFn` ((TyList $ TyVar "a") `TyFn` (TyVar "b")))
 where p = TyVar "b" `TyFn` (TyVar "a" `TyFn` TyVar "b")
primTy _ PrimCase = return $ Forall ["x", "y", "z"] $ (TySum (TyVar "x") (TyVar "y")) `TyFn` (l `TyFn` (r `TyFn` (TyVar "z")))
 where l = (TyVar "x") `TyFn` (TyVar "z")
       r = (TyVar "y") `TyFn` (TyVar "z")


-----------------------------
-- Substitution

type Subst = [(Var, MTy)]

idSubst :: Subst
idSubst = []

o :: Subst -> Subst -> Subst
o f g = addExtra ++ map h g
 where h (v, g') = (v, subst f g')
       addExtra = filter (\(v,f')-> case lookup v g of
                                      Just y  -> False
                                      Nothing -> True) f

class Substable a where
  subst :: Subst -> a -> a

instance Substable MTy where
 subst f (TyLit lt) = TyLit lt
 subst f (TyVariant ts) = TyVariant $ map (subst f) ts
 subst f (TyTuple ts) = TyTuple $ map (subst f) ts
 subst f TyUnit = TyUnit
 subst f TyVoid = TyVoid
 subst f (TyList t) = TyList $ subst f t
 subst f (TyFn t1 t2) = TyFn (subst f t1) (subst f t2)
 subst f (TyProd t1 t2) = TyProd  (subst f t1) (subst f t2)
 subst f (TySum t1 t2) = TySum  (subst f t1) (subst f t2)
 subst f (TyVar v) = case lookup v f of
                      Nothing -> TyVar v
                      Just y -> y
 subst f (TyCon v ts) = TyCon v $ map (subst f) ts

instance Substable FTy where
 subst f (FTyLit lt) = FTyLit lt
 subst f (FTyVariant ts) = FTyVariant $ map (subst f) ts
 subst f (FTyTuple ts) = FTyTuple $ map (subst f) ts
 subst f FTyUnit = FTyUnit
 subst f FTyVoid = FTyVoid
 subst f (FTyList t) = FTyList $ subst f t
 subst f (FTyFn t1 t2) = FTyFn (subst f t1) (subst f t2)
 subst f (FTyProd t1 t2) = FTyProd  (subst f t1) (subst f t2)
 subst f (FTySum t1 t2) = FTySum  (subst f t1) (subst f t2)
 subst f (FTyVar v) = case lookup v f of
                        Nothing -> FTyVar v
                        Just y -> mTyToFTy y
 subst f (FTyForall vs t) = FTyForall vs $ subst phi' t
  where phi' = filter (\(v,f')-> not (elem v vs)) f
 subst f (FTyCon v ts) = FTyCon v $ map (subst f) ts

instance Substable TypSch where
 subst f (Forall vs t) = Forall vs $ subst f' t
   where f' = filter (\(v,t')-> not $ elem v vs) f

instance Substable Ctx where
 subst phi g = map (\(k,v)->(k, subst phi v)) g

instance Substable FExpr where
 subst phi (FInj i js e) = FInj i (map (subst phi) js) $ subst phi e
 subst phi (FProj i e) = FProj i $ subst phi e
 subst phi (FTuple es) = FTuple $ map (subst phi) es
 subst phi (FCase e t es) = FCase (subst phi e) (subst phi t) $ map (subst phi) es
 subst phi (FConst p) = FConst p
 subst phi (FVar p) = FVar p
 subst phi (FApp p q) = FApp (subst phi p) (subst phi q)
 subst phi (FAbs p t q) = FAbs p (subst phi t) (subst phi q)
 subst phi (FTyApp p q) = FTyApp (subst phi p) (map (subst phi) q)
 subst phi (FTyAbs vs p) = FTyAbs vs (subst phi' p)
  where phi' = filter (\(v,f')-> not (elem v vs)) phi
 subst phi (FLetrec vs p) = FLetrec (map (\(k,t,v)->(k,subst phi t, subst phi v)) vs) (subst phi p)

subst' :: [(Var,FTy)] -> FTy -> FTy
subst' f (FTyLit lt) = FTyLit lt
subst' f (FTyTuple ts) = FTyTuple $ (map $ subst' f) ts
subst' f (FTyVariant ts) = FTyVariant $ (map $ subst' f) ts
subst' f FTyUnit = FTyUnit
subst' f FTyVoid = FTyVoid
subst' f (FTyList t) = FTyList $ subst' f t
subst' f (FTyFn t1 t2) = FTyFn (subst' f t1) (subst' f t2)
subst' f (FTyProd t1 t2) = FTyProd  (subst' f t1) (subst' f t2)
subst' f (FTySum t1 t2) = FTySum  (subst' f t1) (subst' f t2)
subst' f (FTyVar v) = case lookup v f of
                        Nothing -> FTyVar v
                        Just y -> y
subst' f (FTyForall vs t) = FTyForall vs $ subst' f' t
  where f' = filter (\(v,f')-> not (elem v vs)) f
subst' f (FTyCon v ts) = FTyCon v $ map (subst' f) ts

instance Show Ctx where
  show [] = ""
  show ((k,v):t) = k ++ ":" ++ show v ++ " " ++ show t

------------------------------------
-- Type checking for F

open :: [Var] -> [FTy] -> FTy -> Either String FTy
open vs ts e | length vs == length ts = return $ subst' (zip vs ts) e
             | otherwise = throwError "Cannot open"

typeOf :: ADTs -> [Var] -> [(Var,FTy)] -> FExpr -> Either String FTy
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
typeOf adts tvs g (FVar x) = case lookup x g of
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
typeOf adts tvs g (FAbs x t e) = do { t1 <- typeOf adts tvs ((x,t):g) e
                                    ; return $ t `FTyFn` t1 }
typeOf adts tvs g (FTyAbs vs e) = do { t1 <- typeOf adts (vs++tvs) g e
                                     ; return $ FTyForall vs t1 }
typeOf adts tvs g (FTyApp e ts) = do { t1 <- typeOf adts tvs g e
                                     ; case t1 of
                                        FTyForall vs t -> open vs ts t
                                        v -> throwError $ "not a forall type: " ++ show v }
typeOf adts tvs g (FLetrec es e) = do { let g' = map (\(k,t,e)->(k,t)) es
                                      ; est <- mapM (\(_,_,v)->typeOf adts tvs (g'++g) v) es
                                      ; if est == (snd $ unzip g')
                                        then typeOf adts tvs g' e
                                        else throwError $ "Disagree: " ++ show est ++ " and " ++ (show $ snd $ unzip g') }


-----------------------------
-- Unification

mgu :: MTy -> MTy -> E Subst
mgu (TyLit lt1) (TyLit lt2) = if lt1 == lt2
  then return []
  else throwError $ "Cannot unify literal types " ++ show lt1 ++ " and " ++ show lt2
mgu (TyVariant ts1) (TyVariant ts2) | length ts1 == length ts2 = mgu' ts1 ts2
                                    | otherwise = throwError $ "cannot unify " ++ show ts1 ++ " with " ++ show ts2
mgu (TyTuple ts1) (TyTuple ts2) | length ts1 == length ts2 = mgu' ts1 ts2
                                | otherwise = throwError $ "cannot unify " ++ show ts1 ++ " with " ++ show ts2
mgu (TyList a) (TyList b) = mgu a b
mgu TyUnit TyUnit = return []
mgu TyVoid TyVoid = return []
mgu (TyCon a as) (TyCon b bs) | a == b && length as == length bs = mgu' as bs
                              | otherwise = throwError $ "cannot unify " ++ show a ++ " with " ++ show b
mgu (TyProd a b) (TyProd a' b') = do { s <- mgu a a' ; s' <- mgu (subst s b) (subst s b'); return $ s' `o` s }
mgu (TySum  a b) (TySum  a' b') = do { s <- mgu a a' ; s' <- mgu (subst s b) (subst s b'); return $ s' `o` s }
mgu (TyFn   a b) (TyFn   a' b') = do { s <- mgu a a' ; s' <- mgu (subst s b) (subst s b'); return $ s' `o` s }
mgu (TyVar a) (TyVar b) | a == b = return []
mgu (TyVar a) b = do { occurs a b; return [(a, b)] }
mgu a (TyVar b) = mgu (TyVar b) a
mgu a b = throwError $ "cannot unify " ++ show a ++ " with " ++ show b

mgu' :: [MTy] -> [MTy] -> E Subst
mgu' [] [] = return idSubst
mgu' (a:as) (b:bs) = do { f <- mgu a b; s <- mgu' (map (subst f) as) (map (subst f) bs); return $ s `o` f }

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
type M a = E (Subst, a)

log0 :: Int -> String -> E ()
log0 i x = do { (l, s) <- get; put ((x'++x):l, s); }
 where x' = foldl (\p q -> "\t" ++ p) "" [0..i]

fresh :: E MTy
fresh = do { (l,s) <- get; put (l, s + 1); return $ TyVar $ "v" ++ show s }

inst :: TypSch -> E (MTy,[MTy])
inst (Forall vs ty) = do { vs' <- mapM (\_->fresh) vs; return $ (subst (zip vs vs') ty,  vs') }

gen :: Ctx -> MTy -> (TypSch, [Var])
gen g t = (Forall vs t , vs)
 where vs = nub $ filter (\v -> not $ elem v (vars g)) (vars t)

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
 where g' = map (\(k,v)->(k, tyToFTy v)) g

w :: Int -> ADTs -> Ctx -> Expr -> M (MTy, FExpr)
w l adts g (ExprCase e es') = do { t <- fresh
                            ; bs' <- mapM (\_->fresh) es'
                            ; let bs = map (\k-> TyFn k t) bs'
                            ; (phi , (ts, es)) <- w' (l+1) g es'
                            ; m1 <- mgu' bs ts
                            ; (phi', (t', e')) <- w  (l+1) adts (subst (m1 `o` phi) g) e
                            ; m2 <- mgu (subst (m1 `o` phi) t') (subst (m1 `o` phi' `o` phi) $ TyVariant bs')
                            ; let ret = FCase (subst (m2 ) e') (mTyToFTy ret_t) (map (subst $ m2 `o` phi' `o` m1) es)
                                  ret_t = subst ret_subst t
                                  ret_subst = m2 `o` phi' `o` m1 `o` phi
                            ; checkAgainstF adts (subst ret_subst g) ret_t ret
                            ; return (ret_subst, (ret_t, ret)) }
 where w' l g [] = return (idSubst, ([], []))
       w' l g (e:tl) = do { (u,(u', j)) <- w l adts g e
                        ; (r,(r', h)) <- w' l (subst u g ) tl
                        ; return (r `o` u, ((subst r u'):r', (subst r j):h)) }
w l adts g (ExprInj i j e) = do {
                     ; (s0, (t0, a)) <- w (l+1) adts g e
                     ; t'' <- mapM (\_->fresh) [1..j]
                     ; let t' = replace t'' (i, t0)
                     ; let ret = FInj i (map (mTyToFTy) t') a
                           ret_t = TyVariant t'
                           ret_subst = s0
                     ; log0 l $ "INJ " ++ show (subst ret_subst g) ++ "|- " ++ (show $ ret) ++ " : " ++  show ret_t
                     ; checkAgainstF adts (subst ret_subst g) ret_t ret
                     ; return (ret_subst, (ret_t, ret)) }
w l adts g (ExprProj i j e) = do {
                     ; (s0, (t0, a)) <- w (l+1) adts g e
                     ; t' <- mapM (\_->fresh) [1..j]
                     ; s2 <-  t0 `mgu` (TyTuple t')
                     ; let ret = FProj i (subst s2 a)
                           ret_t = subst s2 (t' !! i)
                           ret_subst = s2 `o` s0
                     ; log0 l $ "PROJ " ++ show (subst ret_subst g) ++ "|- " ++ (show $ ret) ++ " : " ++  show ret_t
                     ; checkAgainstF adts (subst ret_subst g) ret_t ret
                     ; return (ret_subst, (ret_t, ret)) }
w l adts g (ExprTuple es') = do { (phi, (ts, es)) <- w' (l+1) g es'
                            ; let ret = FTuple es
                                  ret_t = TyTuple ts
                                  ret_subst = phi
                            ; log0 l $ "TUPLE " ++ show (subst ret_subst g) ++ "|- " ++ (show $ ret) ++ " : " ++  show ret_t
                            ; checkAgainstF adts (subst ret_subst g) ret_t ret
                            ; return (ret_subst, (ret_t, ret)) }
 where w' l g [] = return (idSubst, ([], []))
       w' l g (e:tl) = do { (u,(u', j)) <- w l adts g e
                        ; (r,(r', h)) <- w' l (subst u g ) tl
                        ; return (r `o` u, ((subst r u'):r', (subst r j):h)) }
w l adts g (ExprConst p) = do { case primTy adts p of
                              Left er -> throwError er
                              Right t -> do { (ret_t, vs) <- inst t
                        ; let ret = fTyApp (FConst p) $ map mTyToFTy vs
                          ; log0 l $ "CONST " ++ show g ++ "|- " ++  show ret ++ " : " ++ show ret_t
                        ; checkAgainstF adts g ret_t ret
                        ; return (idSubst, (ret_t, ret)) } }
w l adts g (ExprVar x) = case lookup x g of
                Nothing -> throwError $ "Unknown var: " ++ (show x)
                Just s -> do { (ret_t, vs) <- inst s
                             ; let ret = fTyApp (FVar x) $ map mTyToFTy vs
                             ; log0 l $ "VAR " ++ show g ++ "|- " ++ show ret ++ " : " ++ show ret_t
                             ; checkAgainstF adts g ret_t ret
                             ; return (idSubst, (ret_t, ret)) }
w l adts g (ExprApp e0 e1) = do {
                     ; (s0, (t0, a)) <- w (l+1) adts g e0
                     ; (s1, (t1, b)) <- w (l+1) adts (subst s0 g) e1
                     ; t' <- fresh
                     ; s2 <-  (subst s1 t0) `mgu` (t1 `TyFn` t')
                     ; let ret = FApp (subst (s2 `o` s1) a) (subst s2 b)
                           ret_t = subst s2 t'
                           ret_subst = s2 `o` (s1 `o` s0);
                     ; log0 l $ "APP " ++ show (subst ret_subst g) ++ "|- " ++ show ret ++ " : " ++  show ret_t
                     ; checkAgainstF adts (subst ret_subst g) ret_t ret
                     ; return (ret_subst, (ret_t, ret)) }
w l adts g (ExprAbs x e) = do {
                        ; t  <- fresh
                        ; (ret_subst, (t', a)) <- w (l+1) adts ((x, (Forall [] t)):g) e
                        ; let ret = FAbs x (mTyToFTy $ subst ret_subst t) a
                              ret_t = TyFn (subst ret_subst t) t'
                        ; log0 l $ "ABS " ++ show (subst ret_subst g) ++ "|- \\" ++ x ++ ". " ++ show ret ++ " : " ++ show ret_t
                        ; checkAgainstF adts (subst ret_subst g) ret_t ret
                        ; return (ret_subst, (ret_t, ret)) }
w l adts g (ExprLetrec xe0 e1) = do {
                         ; t0s <- mapM (\(k,v) -> do { f <- fresh; return (k, f) }) xe0
                         ; let g' = map (\(k,v) -> (k, Forall [] v)) t0s ++ g
                         ; (s0, (ts,e0XS)) <- w' (l+1) g' xe0
                         ; s' <- mgu' (map (\(_,v) -> subst s0 v) t0s) ts
                         ; let g'' = subst (s' `o` s0) g'
                               g'''  = map (\(k,t) -> (k, fst $ gen g (subst s' t))) $ zip (fst $ unzip xe0) ts
                         ; (s1, (t',e1X)) <- w (l+1) adts (g''') e1
                         ; let g''X = map (\(k,t) -> (k, gen g (subst (s') t))) $ zip (fst $ unzip xe0) ts
                               e0Xs = map (subst (s1 `o` s')) e0XS
                               mmm = map (\((x,(ww,ww2)),e0X)->(x, (fTyApp (FVar x) $ map FTyVar ww2))) $  zip g''X e0Xs
                               e0X's =  map (\((x,(ww,ww2)),e0X)->(x,ww,ww2, subst'' mmm e0X)) $ zip g''X e0Xs
                               e0X''s = map (\(x,ww,ww2,e) -> (x,ww,ww2,fTyAbs ww2 e)) e0X's
                               bs = map (\(x,ww,ww2,e0X'') -> (x, subst s1 $ tyToFTy ww, subst (s' `o` s1) e0X'')) e0X''s
                               ret = FLetrec bs e1X
                               ret_t = t'
                               ret_subst = s1 `o` s' `o` s0
                        ; log0 l $ "LETREC " ++ show (subst ret_subst g) ++ "|- " ++ show ret ++ " : " ++ show ret_t
                        ; checkAgainstF adts (subst ret_subst g) ret_t ret
                        ; return (ret_subst, (ret_t, ret)) }
 where w' l g [] = return (idSubst, ([], []))
       w' l g  ((k,v):tl) = do { (u,(u', j)) <- w l adts g v
                             ; (r,(r', h)) <- w' l (subst u g ) tl
                             ; return (r `o` u, ((subst r u'):r', (subst r j):h)) }


subst'' :: [(Var, FExpr)] -> FExpr -> FExpr
subst'' phi (FTuple es) = FTuple $ map (subst'' phi) es
subst'' phi (FConst c) = FConst c
subst'' phi (FVar v') = case lookup v' phi of
                         Just y -> y
                         Nothing -> FVar v'
subst'' phi (FApp a b) = FApp (subst'' phi a) (subst'' phi b)
subst'' phi (FAbs v' a b) = FAbs v' a $ subst'' phi' b
 where phi' = filter (\(k,v) -> not (k == v')) phi
subst'' phi (FTyApp a ts) = FTyApp (subst'' phi a) ts
subst'' phi (FTyAbs vs a) = FTyAbs vs $ subst'' phi a
subst'' phi (FLetrec es e) = FLetrec (map (\(k,t,f)->(k,t,subst'' phi' f)) es) (subst'' phi' e)
 where phi' = filter (\(k,v) -> not (elem k ns)) phi
       (ns,ts,es') = unzip3 es

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
               ; let out = runState (runExceptT (w 0 theAdts [] t)) ([],0)
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
