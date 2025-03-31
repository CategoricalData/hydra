{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

-- Implementation of Hindley Milner algorithm W to system F translation by Ryan Wisnesky.
-- Lightweight adaptation to Hydra by Joshua Shinavier.
-- License: Apache 2.0 https://www.apache.org/licenses/LICENSE-2.0 

module Hydra.Reference.AlgorithmW where

import Prelude
import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Debug.Trace

import Hydra.Minimal

natType = TyLit $ LiteralTypeInteger IntegerTypeInt32
constNeg = Const $ PrimTyped $ TypedPrimitive (Name "hydra.lib.math.neg") $ Forall [] $ TyFn natType natType
-- Note: Hydra has no built-in pred or succ functions, but neg has the expected type
constPred = constNeg
constSucc = constNeg
nat = Const . PrimLiteral . int32
str = Const . PrimLiteral . string

-- A typed primitive corresponds to the Hydra primitive of the same name
data TypedPrimitive = TypedPrimitive Name TypSch deriving (Eq, Show)

------------------------
-- STLC

type Var = String

data Prim
 = PrimLiteral Literal
 | PrimTyped TypedPrimitive
 | Succ | Pred | If0
 | Fst | Snd | Pair | TT
 | Nil | Cons | FoldList  
 | FF | Inl | Inr | Case 
 deriving (Eq, Show)
 
showPrim :: Prim -> String
showPrim (PrimLiteral l) = case l of
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
showPrim (PrimTyped (TypedPrimitive name _)) = unName name ++ "()"
showPrim Succ = "S"
showPrim Pred = "P"
showPrim FoldList = "fold"
showPrim Fst = "fst"
showPrim Snd = "snd"
showPrim Nil = "nil"
showPrim Cons = "cons"
showPrim TT = "tt"
showPrim FF = "ff"
showPrim Inl = "inl"
showPrim Inr = "inr"
showPrim Case = "case"
showPrim If0 = "if0"
showPrim Pair = "pair"

data Expr = Const Prim
 | Var Var
 | App Expr Expr
 | Abs Var Expr
 | Letrec [(Var, Expr)] Expr
 deriving (Eq, Show)

showExpr :: Expr -> String
showExpr (Const p) = showPrim p
showExpr (Var v) = v
showExpr (App (App (App a' a) b) b') = "(" ++ showExpr a' ++ " " ++ showExpr a ++ " " ++ showExpr b ++ " " ++ showExpr b' ++ ")"
showExpr (App (App a b) b') = "(" ++ showExpr a ++ " " ++ showExpr b ++ " " ++ showExpr b' ++ ")"
showExpr (App a b) = "(" ++ showExpr a ++ " " ++ showExpr b ++ ")"
showExpr (Abs a b) = "(\\" ++ a ++ ". " ++ showExpr b ++ ")"
showExpr (Letrec ab c) = "let " ++ d ++ showExpr c
  where d = foldr (\(p, q) r -> p ++ " = " ++ showExpr q ++ " \n\t\t" ++ r) "in " ab

data MTy = TyVar Var
  | TyLit LiteralType
  | TyList MTy
  | TyFn MTy MTy
  | TyProd MTy MTy
  | TySum MTy MTy
  | TyUnit 
  | TyVoid
 deriving (Eq, Show)
 
showMTy :: MTy -> String
showMTy (TyLit lt) = case lt of
  LiteralTypeInteger it -> drop (length "IntegerType") $ show it
  LiteralTypeFloat ft -> drop (length "FloatType") $ show ft
  _ -> drop (length "LiteralType") $ show lt
showMTy (TyVar v) = v
showMTy (TyList t) = "(List " ++ (showMTy t) ++ ")"
showMTy (TyFn t1 t2) = "(" ++ showMTy t1 ++ " -> " ++ showMTy t2 ++ ")"
showMTy (TyProd t1 t2) = "(" ++ showMTy t1 ++ " * " ++ showMTy t2 ++  ")"
showMTy (TySum t1 t2) = "(" ++ showMTy t1 ++ " + " ++ showMTy t2 ++  ")"
showMTy TyUnit = "Unit"
showMTy TyVoid = "Void"
  
instance Show TypSch where
  show (Forall [] t) = show t
  show (Forall x t) = "forall " ++ d ++ show t
   where d = foldr (\p q -> p ++ " " ++ q) ", " x
  
data TypSch = Forall [Var] MTy
 deriving Eq 

------------------------
-- System F

data FExpr = FConst Prim
 | FVar Var
 | FApp FExpr FExpr
 | FAbs Var FTy FExpr
 | FTyApp FExpr [FTy]
 | FTyAbs [Var] FExpr 
 | FLetrec [(Var, FTy, FExpr)] FExpr
 deriving (Eq, Show)

showFExpr :: FExpr -> String
showFExpr (FConst p) = showPrim p
showFExpr (FVar v) = v
showFExpr (FTyApp e t) = "(" ++ showFExpr e ++ " " ++ show (showFTy <$> t) ++ ")"
showFExpr (FApp (FApp (FApp a' a) b) b') = "(" ++ showFExpr a' ++ " " ++ showFExpr a ++ " " ++ showFExpr b ++ " " ++ showFExpr b' ++ ")"
showFExpr (FApp (FApp a b) b') = "(" ++ showFExpr a ++ " " ++ showFExpr b ++ " " ++ showFExpr b' ++ ")"
showFExpr (FApp a b) = "(" ++ showFExpr a ++ " " ++ showFExpr b ++ ")"
showFExpr (FAbs a t b) = "(\\" ++ a ++ ":" ++ showFTy t ++ ". " ++ showFExpr b ++ ")"
showFExpr (FLetrec ab c) = "let " ++ d ++ showFExpr c
  where d = foldr (\(p, t, q) r -> p ++ ":" ++ showFTy t ++ " = " ++ showFExpr q ++ " \n\t\t" ++ r) "in " ab
showFExpr (FTyAbs ab c) = "(/\\" ++ d ++ showFExpr c ++ ")"
  where d = foldr (\p r -> p ++ " " ++ r) ". " ab

data FTy = FTyVar Var 
  | FTyLit LiteralType
  | FTyList FTy
  | FTyFn FTy FTy
  | FTyProd FTy FTy
  | FTySum FTy FTy
  | FTyUnit 
  | FTyVoid
  | FForall [Var] FTy
 deriving (Eq, Show)
 
showFTy :: FTy -> String
showFTy (FTyLit lt) = showMTy $ TyLit lt
showFTy (FTyVar v) = v
showFTy (FTyList t) = "(List " ++ (showFTy t) ++ ")"
showFTy (FTyFn t1 t2) = "(" ++ showFTy t1 ++ " -> " ++ showFTy t2 ++ ")"
showFTy (FTyProd t1 t2) = "(" ++ showFTy t1 ++ " * " ++ showFTy t2 ++  ")"
showFTy (FTySum t1 t2) = "(" ++ showFTy t1 ++ " + " ++ showFTy t2 ++  ")"
showFTy FTyUnit = "Unit"
showFTy FTyVoid = "Void"
showFTy (FForall x t) = "(forall " ++ d ++ showFTy t ++ ")"
 where d = foldr (\p q -> p ++ " " ++ q) ", " x

mTyToFTy :: MTy -> FTy 
mTyToFTy (TyVar v) = FTyVar v 
mTyToFTy (TyLit lt) = FTyLit lt
mTyToFTy TyUnit = FTyUnit
mTyToFTy TyVoid = FTyVoid
mTyToFTy (TyList x) = FTyList $ mTyToFTy x
mTyToFTy (TyFn x y) = FTyFn (mTyToFTy x) (mTyToFTy y)
mTyToFTy (TyProd x y) = FTyProd (mTyToFTy x) (mTyToFTy y)
mTyToFTy (TySum x y) = FTySum (mTyToFTy x) (mTyToFTy y)

tyToFTy :: TypSch -> FTy 
tyToFTy (Forall [] t) = mTyToFTy t
tyToFTy (Forall vs t) = FForall vs (mTyToFTy t)

--------------------
-- Contexts 

type Ctx  = [(Var, TypSch)]
type FCtx = [(Var, FTy)]

instance Show Ctx
 where show [] = ""
       show ((v,t):[]) = v ++ ":" ++ show t ++ "  "
       show ((v,t):x ) = v ++ ":" ++ show t ++ " " ++ show x

instance Show FCtx
 where show [] = ""
       show ((v,t):[]) = v ++ ":" ++ show t
       show ((v,t):x ) = v ++ ":" ++ show t ++ " " ++ show x

class Vars a where
  vars :: a -> [Var]

instance Vars Ctx where
 vars [] = []
 vars ((v,t):l) = vars t ++ vars l

instance Vars TypSch where
 vars (Forall vs t) = filter (\v -> not $ elem v vs) (vars t) 

instance Vars MTy where
 vars (TyVar v) = [v] 
 vars (TyList t) = vars t 
 vars (TyFn t1 t2) = vars t1 ++ vars t2
 vars TyUnit = []
 vars TyVoid = []
 vars (TyProd t1 t2) = vars t1 ++ vars t2
 vars (TySum t1 t2) = vars t1 ++ vars t2
 vars (TyLit _) = []

primTy :: Prim -> TypSch
primTy (PrimLiteral l) = Forall [] $ TyLit $ literalType l
primTy (PrimTyped (TypedPrimitive _ forAll)) = forAll
primTy Fst = Forall ["x", "y"] $ (TyProd (TyVar "x") (TyVar "y")) `TyFn` (TyVar "x")
primTy Snd = Forall ["x", "y"] $ (TyProd (TyVar "x") (TyVar "y")) `TyFn` (TyVar "y")
primTy Nil = Forall ["t"] $ TyList (TyVar "t")
primTy Cons = Forall ["t"] $ TyFn (TyVar "t") (TyFn (TyList (TyVar "t")) (TyList (TyVar "t")))
primTy TT = Forall [] TyUnit
primTy FF = Forall ["t"] $ TyFn TyVoid (TyVar "t")
primTy Inl = Forall ["x", "y"] $ (TyVar "x") `TyFn` (TyProd (TyVar "x") (TyVar "y"))  
primTy Inr = Forall ["x", "y"] $ (TyVar "y") `TyFn` (TyProd (TyVar "x") (TyVar "y"))
primTy Succ = Forall [] $ natType `TyFn` natType
primTy Pred = Forall [] $ natType `TyFn` natType
primTy Pair = Forall ["x", "y"] $ (TyFn (TyVar "x") (TyFn (TyVar "y") (TyProd (TyVar "x") (TyVar "y"))))
primTy If0 = Forall [] $ natType `TyFn` (natType `TyFn` (natType `TyFn` natType))
primTy FoldList = Forall ["a", "b"] $ p `TyFn` ((TyVar "b") `TyFn` ((TyList $ TyVar "a") `TyFn` (TyVar "b")))
 where p = TyVar "b" `TyFn` (TyVar "a" `TyFn` TyVar "b")
primTy Case = Forall ["x", "y", "z"] $ (TySum (TyVar "x") (TyVar "y")) `TyFn` (l `TyFn` (r `TyFn` (TyVar "z"))) 
 where l = (TyVar "x") `TyFn` (TyVar "z")
       r = (TyVar "y") `TyFn` (TyVar "z")
  
ctxToFCtx :: Ctx -> [(Var, FTy)]
ctxToFCtx [] = []
ctxToFCtx ((k,v):b) = (k, (tyToFTy v)) : ctxToFCtx b

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
 subst f TyUnit = TyUnit
 subst f TyVoid = TyVoid
 subst f (TyList t) = TyList $ subst f t
 subst f (TyFn t1 t2) = TyFn (subst f t1) (subst f t2)
 subst f (TyProd t1 t2) = TyProd  (subst f t1) (subst f t2)
 subst f (TySum t1 t2) = TySum  (subst f t1) (subst f t2)
 subst f (TyVar v) = case lookup v f of
                      Nothing -> TyVar v
                      Just y -> y
                      
instance Substable FTy where
 subst f (FTyLit lt) = FTyLit lt
 subst f FTyUnit = FTyUnit
 subst f FTyVoid = FTyVoid
 subst f (FTyList t) = FTyList $ subst f t 
 subst f (FTyFn t1 t2) = FTyFn (subst f t1) (subst f t2) 
 subst f (FTyProd t1 t2) = FTyProd  (subst f t1) (subst f t2)
 subst f (FTySum t1 t2) = FTySum  (subst f t1) (subst f t2)
 subst f (FTyVar v) = case lookup v f of
                        Nothing -> FTyVar v
                        Just y -> mTyToFTy y
 subst f (FForall vs t) = FForall vs $ subst phi' t                        
  where phi' = filter (\(v,f')-> not (elem v vs)) f

instance Substable TypSch where
 subst f (Forall vs t) = Forall vs $ subst f' t 
   where f' = filter (\(v,t')-> not $ elem v vs) f

instance Substable Ctx where
 subst phi g = map (\(k,v)->(k, subst phi v)) g

instance Substable FExpr where 
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
subst' f FTyUnit = FTyUnit
subst' f FTyVoid = FTyVoid
subst' f (FTyList t) = FTyList $ subst' f t 
subst' f (FTyFn t1 t2) = FTyFn (subst' f t1) (subst' f t2) 
subst' f (FTyProd t1 t2) = FTyProd  (subst' f t1) (subst' f t2)
subst' f (FTySum t1 t2) = FTySum  (subst' f t1) (subst' f t2)
subst' f (FTyVar v) = case lookup v f of
                        Nothing -> FTyVar v
                        Just y -> y
subst' f (FForall vs t) = FForall vs $ subst' f' t                         
 where f' = filter (\(v,f')-> not (elem v vs)) f
 
------------------------------------
-- Type checking for F

open :: [Var] -> [FTy] -> FTy -> Either String FTy
open vs ts e | length vs == length ts = return $ subst' (zip vs ts) e
             | otherwise = throwError "Cannot open"


wfTy :: [Var] -> FTy -> Either String ()
wfTy tvs x = case x of
                FTyLit _ -> return ()
                FTyList y -> wfTy tvs y
                FTyFn w v -> wfTy tvs w >> wfTy tvs v
                FTyProd w v -> wfTy tvs w >> wfTy tvs v
                FTySum w v -> wfTy tvs w >> wfTy tvs v
                FTyUnit -> return ()
                FTyVoid -> return ()
                FForall vs y -> wfTy (vs++tvs) y
                FTyVar v -> if elem v tvs then return () else throwError $ "unbound tyvar " ++ v ++ " in " ++ show tvs


typeOf :: [Var] -> [(Var,FTy)] -> FExpr -> Either String FTy
typeOf tvs g (FVar x) = case lookup x g of
  Nothing -> throwError $ "unbound var: " ++ x ++ " in ctx " ++ show g
  Just y -> return y
typeOf tvs g (FConst p) = return $ tyToFTy $ primTy p
typeOf tvs g (FApp a b) = do { t1 <- typeOf tvs g a
                             ; t2 <- typeOf tvs g b
                             ; wfTy tvs t1
                             ; wfTy tvs t2
                             ; case t1 of
                                (FTyFn p q) -> if p == t2
                                               then return q
                                               else throwError $ "3In " ++ (show $ FApp a b) ++ " expected " ++ show p ++ " given " ++ show t2
                                v -> throwError $ "4In " ++ show g ++ " |- " ++ show (FApp a b) ++ " not a fn type: " ++ show v }
typeOf tvs g (FAbs x t e) = do { wfTy tvs t
                               ; t1 <- typeOf tvs ((x,t):g) e
                               ; wfTy tvs t1
                               ; return $ t `FTyFn` t1 }
typeOf tvs g (FTyAbs vs e) = do { t1 <- typeOf (vs++tvs) g e
                                ; wfTy (vs++tvs) t1
                                ; return $ FForall vs t1 }
typeOf tvs g (FTyApp e ts) = do { t1 <- typeOf tvs g e
                                ; wfTy tvs t1
                                ; case t1 of
                                    FForall vs t -> open vs ts t
                                    v -> throwError $ "not a forall type: " ++ show v }
typeOf tvs g (FLetrec es e) = do { let g' = map (\(k,t,e)->(k,t)) es
                                 ; est <- mapM (\(_,_,v)->typeOf tvs (g'++g) v) es
                                 ; mapM (wfTy tvs) est
                                 ; mapM (wfTy tvs) $ snd $ unzip g'
                                 ; if est == (snd $ unzip g')
                                   then typeOf tvs (g'++g) e
                                   else throwError $ "Disagree: " ++ show est ++ " and " ++ (show $ snd $ unzip g') }


-----------------------------
-- Unification

mgu :: MTy -> MTy -> E Subst
mgu (TyLit lt1) (TyLit lt2) = if lt1 == lt2
  then return []
  else throwError $ "Cannot unify literal types " ++ show lt1 ++ " and " ++ show lt2
mgu (TyList a) (TyList b) = mgu a b
mgu TyUnit TyUnit = return []
mgu TyVoid TyVoid = return []
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

type E = ExceptT String (State Integer)
type M a = E (Subst, a)

fresh :: E MTy
fresh = do { s <- get; put (s + 1); return $ TyVar $ "v" ++ show s }

inst :: TypSch -> E (MTy, [MTy])
inst (Forall vs ty) = do { vs' <- mapM (\_->fresh) vs; return $ (subst (zip vs vs') ty,  vs') }

gen :: Ctx -> MTy -> (TypSch, [Var])
gen g t = (Forall vs t , vs)
 where vs = nub $ filter (\v -> not $ elem v (vars g)) (vars t)

fTyApp x [] = x
fTyApp x y = FTyApp x y

fTyAbs [] x = x
fTyAbs x y = FTyAbs x y

check :: [Var] -> Ctx -> MTy -> FExpr -> M ()
check k g t e = let ret = typeOf k (ctxToFCtx g) e
                in case ret of
                  Right t0 -> if t0 == mTyToFTy t
                              then return (idSubst, ())
                              else throwError $ "2In\n" ++ show k ++ ", " ++ show g ++
                                 " |- " ++ show e ++ " : " ++ show t ++ "\n " ++ show t0 ++ " is computed"
                  Left err -> throwError $ "1In\n" ++ show k ++ ", " ++ show g ++
                                 " |- " ++ show e ++ " : " ++ show t ++ "\n" ++ err

check0 :: [Var] -> Ctx -> [MTy] -> [FExpr] -> M ()
check0 k g [] [] = return (idSubst, ())
check0 k g (a:b) (c:d) = check k g a c >> check0 k g b d

w :: Ctx -> Expr -> M (MTy, FExpr)
w g (Const p) = do { (t,vs) <- inst $ primTy p
                     ; let ret = (idSubst, (t, fTyApp (FConst p) $ map mTyToFTy vs))
                     ; check (map (\(TyVar a )->a) vs) (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
                     ; return ret }
 where Forall vs t' = primTy p
w g (Var x) = case lookup x g of
                Nothing -> throwError $ "Unknown var: " ++ (show x) ++ " in ctx " ++ (show g)
                Just s -> do { (t, vs) <- inst s
                             ; let ret = (idSubst, (t, fTyApp (FVar x) $ map mTyToFTy vs))
                             ; check (map (\(TyVar a )->a) vs) (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
                             ; return ret }
w g (App e0 e1) = do { (s0, (t0, a)) <- w g e0
                     ; (s1, (t1, b)) <- w (subst s0 g) e1
                     ; t' <- fresh
                     ; s2 <-  (subst s1 t0) `mgu` (t1 `TyFn` t')
                     ; let ret = (s2 `o` (s1 `o` s0), (subst s2 t', FApp (subst (s2 `o` s1) a) (subst s2 b)))
                     ; check ((vars (subst (fst ret) g)) ++ (vars (subst (fst ret) t'))) (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
                     ; return ret; }
w g (Abs x e) = do { t  <- fresh
                   ; (s, (t', a)) <- w ((x, (Forall [] t)):g) e
                   ; let ret = (s, (TyFn (subst s t) t', FAbs x (mTyToFTy $ subst s t) a))
                   ; check ((vars (subst s t)) ++ (vars t') ++ (vars $ subst (fst ret) ((x, (Forall [] t)):g)))
                       (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
                   ; return ret; }
w g (Letrec xe0 e1) = do { t0s <- mapM (\(k,v) -> do { f <- fresh; return (k, f) }) xe0
                         ; let g' = map (\(k,v) -> (k, Forall [] v)) t0s ++ g
                         ; (s0, (ts,e0Xs)) <- w' g' xe0
                         ; s' <- mgu' (map (\(_,v) -> subst s0 v) t0s) ts

                         ; let g''' = subst (s' `o` s0) g
                               g''  = map (\(k,t) -> (k, fst $ gen {--((subst s' g') ++ --} g''' (subst s' t))) $ zip (fst $ unzip xe0) ts
                               g''X = map (\(k,t) -> (k,       gen {--((subst s' g') ++ --} g''' (subst s' t))) $ zip (fst $ unzip xe0) ts
                         ; (s2, (t',e1X)) <- w (g'' ++ g''') e1

                         ; let mmm    = map (\((x,(ww,ww2)),e0X)->(x, (fTyApp (FVar x) $ map FTyVar ww2))) $  zip g''X e0Xs
                         ; let e0X's  = map (\((x,(ww,ww2)),e0X)->(x,ww,ww2, subst'' mmm e0X))             $  zip g''X e0Xs
                         ; let e0X''s = map (\( x, ww,ww2,  e  )->(x,ww,ww2,  fTyAbs ww2 e  ))                         e0X's

                         ; let bs = map (\(x,ww,ww2,e0X'') -> (x, subst s2 $ tyToFTy ww, subst (s' `o` s2) e0X'')) e0X''s
                         ; let ret = (s2 `o` s' `o` s0, (t', FLetrec bs e1X))
                         ; check ((vars $ subst (fst ret) g') ++ (vars t')) (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
                         ; return ret }

 where w' g [] = return (idSubst, ([], []))
       w' g  ((k,v):tl) = do { (u,(u', j)) <- w g v
                             ; (r,(r', h)) <- w' (subst u g ) tl
                             ; let ret = (r `o` u, ((subst r u'):r', (subst r j):h))
                             ; --check0 (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
                             ; return ret }
       


subst'' :: [(Var, FExpr)] -> FExpr -> FExpr
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
 
tests = [testJ, testM, testJ, testB'', testB' , testB, test4, testC, testA, test0, test1, testB, test2, test3a, test5, test6]
-- tests = [testJ, testB'', testB' , testB]


testOne t = do { putStrLn $ "Untyped input: "
               ; putStrLn $ "\t" ++  show t
               ; let out = fst $ runState (runExceptT (w [] t)) 0
               ; case out of
                   Left  e -> putStrLn $ "\t" ++ "err: " ++ e
                   Right (s, (ty, f)) -> do {
                                            ; putStrLn $ "\nType inferred by Hindley-Milner: "
                                            ; putStrLn $ "\t" ++ show ty
                                            ; putStrLn "\nSystem F translation: "
                                            ; putStrLn $ "\t" ++ show f
                                            ; putStrLn "\nSystem F type: "
                                            ; case (typeOf (vars ty) [] f) of
                                               Left err -> putStrLn $ "\t" ++  "err: " ++ err
                                               Right tt -> do { putStrLn $ " \t" ++ show tt
                                                              ; if tt == mTyToFTy ty then return () else putStrLn "**** !!! NO MATCH" } }
               ; putStrLn ""
               ; putStrLn "------------------------"
               ; putStrLn ""  }

stlc = Letrec [("x",Var "y"),("y",Var "x")] (App (App (Const Pair) (Var "x")) (Var "y"))
--main = testOne stlc

yyy = let x = y
          y = x in (x,y)

xxx = let foo = bar
          bar = \f -> f $ bar f
      in (foo, bar)

testM :: Expr
testM = Letrec [("g", Var "f"), ("f", Var "g")] $ App (App (Const Pair) $ Var "f") (Var "g")

testJ :: Expr
testJ = Letrec [ ("bar2", barBody2), ("bar1", barBody) ] $ App (App (Const Pair) (Var "bar1")) (Var "bar2")
 where fooBody = (App (Var "bar1") (Abs "x" $ str "false"))
       barBody = Abs "f" $ App (Var "f") $  (App (Var "bar1") (Var "f"))
       barBody2 = Abs "f" $ App (Var "f") $  (App (Var "bar2") (Var "f"))

letrec' x e f = Letrec [(x,e)] f

testA :: Expr
testA =  letrec' "f" ( (Abs "x" (Var "x"))) $ App (Var "f")  (nat 0)
 where sng0 = App (Var "sng") (nat 0)
       sngAlice = App (Var "sng") (str "alice")
       body = (Var "sng")

test0 :: Expr
test0 =  letrec' "f" (App (Abs "x" (Var "x")) (nat 0)) (Var "f")
 where sng0 = App (Var "sng") (nat 0)
       sngAlice = App (Var "sng") (str "alice")

testB' :: Expr
testB' = (Abs "x" (App (App (Const Cons) (Var "x")) (Const Nil)))

testB''' :: Expr
testB''' = (Abs "x" (( (Var "x")) ))

testB'' :: Expr
testB'' = (Abs "x" ((App (Const Cons) (Var "x")) ))

testB :: Expr
testB = letrec' "sng" (Abs "x" (App (App (Const Cons) (Var "x")) (Const Nil))) body
 where body = (Var "sng")

test1 :: Expr
test1 = letrec' "sng" (Abs "x" (App (App (Const Cons) (Var "x")) (Const Nil))) body
 where sng0 = App (Var "sng") (nat 0)
       sngAlice = App (Var "sng") (str "alice")
       body = App (App (Const Pair) sng0) sngAlice

test2 :: Expr
test2 = letrec' "+" (Abs "x" $ Abs "y" $ recCall) twoPlusOne
 where
   recCall = App (Const Succ) $ App (App (Var "+") (App (Const Pred) (Var "x"))) (Var "y")
   ifz x y z = App (App (App (Const If0) x) y) z
   twoPlusOne = App (App (Var "+") two) one
   two = App (Const Succ) one
   one = App (Const Succ) (nat 0)

testC :: Expr
testC = letrec' "+" (Abs "x" $ Abs "y" $ recCall) $ twoPlusOne
 where
   recCall = App (Const Succ) $ App (App (Var "+") (App (Const Pred) (Var "x"))) ( (Var "y"))
   ifz x y z = App (App (App (Const If0) x) y) z
   twoPlusOne = App (App (Var "+") two) one
   two = App (Const Succ) one
   one = App (Const Succ) (nat 0)

test3 :: Expr
test3 = letrec' "f" f x
 where x =  (Var "f")
       f = Abs "x" $ Abs "y" $ App (App (Var "f") (nat 0)) (Var "x")

test3a :: Expr
test3a = Letrec [("f", f), ("g", g)] x
 where x =  App (App (Const $ Pair) (Var "f")) (Var "g")
       f = Abs "x" $ Abs "y" $ App (App (Var "f") (nat 0)) (Var "x")
       g = Abs "xx" $ Abs "yy" $ App (App (Var "g") (nat 0)) (Var "xx")

test4 :: Expr
test4 = Letrec [("f", f) ,("g", g)] b
 where b = App (App (Const Pair) (Var "f")) (Var "f")
       f = Abs "f" $ Abs "x" $  (App (Var "f") (nat 0))
       g = Abs "u" $ Abs "v" $ App (App (Var "g") (Var "v")) (nat 0)

test4x :: Expr
test4x = Letrec [("f", f) {--,("g", g)--}] (Var "f") --b
 where b = App (App (Const Pair) (Var "f")) (Var "f")
       f = Abs "x" $ Abs "y" $ App (App (Var "f") (nat 0)) (Var "x")
       g = Abs "u" $ Abs "v" $ App (App (Var "g") (Var "v")) (nat 0)

test5 :: Expr
test5 = Letrec [("f", f), ("g", g)] b
 where b = App (App (Const Pair) (Var "f")) (Var "g")
       f = Abs "x" $ Abs "y" $ App (App (Var "g") (nat 0)) (nat 0)
       g = Abs "u" $ Abs "v" $ App (App (Var "f") (Var "v")) (nat 0)


test6 :: Expr
test6 = Letrec [("f", f), ("g", g)] b
 where b = App (App (Const Pair) (Var "f")) (Var "g")
       f = Abs "x" $ Abs "y" $ App (App (Var "g") (nat 0)) (Var "x")
       g = Abs "u" $ Abs "v" $ App (App (Var "f") (nat 0)) (nat 0)
