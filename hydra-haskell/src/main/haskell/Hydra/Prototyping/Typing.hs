{-
import Hydra.Core as HC
import Control.Monad.Except
import Control.Monad.State
import Hydra.Impl.Haskell.Dsl
import qualified Data.Map as M

term expr = Term expr ()

t0 = int32Value 42 :: HC.Term ()
t1 = lambda "x" $ term $ ExpressionOp $ HC.Op BinopAdd t0 (variable "x") :: HC.Term ()

inferType t0
inferType t1

-}

module Hydra.Prototyping.Typing (
  Constraint,
  TypeError(..),
  inferType,
  constraintsExpr,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Prototyping.Basics
import Hydra.Impl.Haskell.Dsl

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type Infer a = ReaderT Gamma (StateT InferState (Except TypeError)) a

type InferState = Int

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

type Gamma = M.Map Variable TypeScheme

type Subst = M.Map TypeVariable Type

startState :: InferState
startState = 0

substType :: M.Map TypeVariable Type -> Type -> Type
substType s typ = case typ of
    TypeElement t -> elementType $ subst t
    TypeFunction (FunctionType dom cod) -> functionType (subst dom) (subst cod)
    TypeList t -> listType $ subst t
    TypeLiteral lt -> TypeLiteral lt
    TypeMap (MapType kt vt) -> mapType (subst kt) (subst vt)
--    TypeNominal name -> TODO
    TypeOptional t -> optionalType $ subst t
    TypeRecord tfields -> recordType (substField <$> tfields)
    TypeSet t -> setType $ subst t
    TypeUnion tfields -> unionType (substField <$> tfields)    
    TypeVariable a -> M.findWithDefault typ a s
  where
    subst = substType s
    substField (FieldType fname t) = FieldType fname $ subst t

freeVarsInType :: Type -> S.Set TypeVariable
freeVarsInType typ = case typ of
  TypeElement t -> freeVarsInType t
  TypeFunction (FunctionType dom cod) -> S.union (freeVarsInType dom) (freeVarsInType cod)
  TypeList t -> freeVarsInType t
  TypeLiteral _ -> S.empty
  TypeMap (MapType kt vt) -> S.union (freeVarsInType kt) (freeVarsInType vt)
--  TypeNominal name -> TODO
  TypeOptional t -> freeVarsInType t
  TypeRecord tfields -> L.foldl S.union S.empty (freeVarsInType . fieldTypeType <$> tfields)
  TypeSet t -> freeVarsInType t
  TypeUnion tfields -> L.foldl S.union S.empty (freeVarsInType . fieldTypeType <$> tfields)
  TypeVariable v -> S.singleton v

--instance Substitutable TypeScheme where
substTypeScheme :: M.Map TypeVariable Type -> TypeScheme -> TypeScheme
substTypeScheme s (TypeScheme as t) = TypeScheme as $ substType s' t
  where
    s' = L.foldr M.delete s as
    
freeVarsInTypeScheme :: TypeScheme -> S.Set TypeVariable
freeVarsInTypeScheme (TypeScheme as t) = S.difference (freeVarsInType t) (S.fromList as)

substConstraint :: Subst -> (Type, Type) -> (Type, Type)
substConstraint s (t1, t2) = (substType s t1, substType s t2)

substList :: (Subst -> a -> a) -> Subst -> [a] -> [a]
substList ap = fmap . ap

freeVarsInList :: (a -> S.Set TypeVariable) -> [a] -> S.Set TypeVariable
freeVarsInList ap = L.foldr (S.union . ap) S.empty

substMap :: (Subst -> b -> b) -> Subst -> M.Map k b -> M.Map k b
substMap ap s = M.map (ap s)

freeVarsInMap :: (a -> S.Set TypeVariable) -> M.Map k a -> S.Set TypeVariable
freeVarsInMap ap env = freeVarsInList ap $ M.elems env

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVariable Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type] deriving Show

-- | Run the inference monad
runInfer :: Gamma -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) startState

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: Gamma -> Term a -> Either TypeError TypeScheme
inferExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ substType subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Gamma -> Term a -> Either TypeError ([Constraint], Subst, Type, TypeScheme)
constraintsExpr env ex = case runInfer env (infer ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ substType subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TypeScheme
closeOver = normalizeTypeScheme . generalize M.empty

-- | Extend type environment
inGamma :: (Variable, TypeScheme) -> Infer a -> Infer a
inGamma (x, sc) m = do
  let scope e = M.insert x sc $ M.delete x e
  local scope m

-- | Lookup type in the environment
lookupGamma :: Variable -> Infer Type
lookupGamma x = do
  env <- ask
  case M.lookup x env of
      Nothing   -> throwError $ UnboundVariable x
      Just s    -> instantiate s

variables :: [String]
variables = (\n -> "v" ++ show n) <$> [1..]

fresh :: Infer Type
fresh = do
    s <- get
    put (s + 1)
    return $ TypeVariable (variables !! s)

instantiate ::  TypeScheme -> Infer Type
instantiate (TypeScheme as t) = do
    as' <- mapM (const fresh) as
    let s = M.fromList $ zip as as'
    return $ substType s t

generalize :: Gamma -> Type -> TypeScheme
generalize env t  = TypeScheme as t
  where
    as = S.toList $ S.difference (freeVarsInType t) (freeVarsInMap freeVarsInTypeScheme env)

binopType :: Binop -> Type
binopType BinopAdd = functionType int32Type (functionType int32Type int32Type)
binopType BinopMul = functionType int32Type (functionType int32Type int32Type)
binopType BinopSub = functionType int32Type (functionType int32Type int32Type)
binopType BinopEql = functionType int32Type (functionType int32Type booleanType)

infer :: Term a -> Infer (Type, [Constraint])
infer term = case termData term of

  ExpressionApplication (Application e1 e2) -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    return (tv, c1 ++ c2 ++ [(t1, functionType t2 tv)])

--  ExpressionElement name -> TODO

  ExpressionFix e1 -> do
    (t1, c1) <- infer e1
    tv <- fresh
    return (tv, c1 ++ [(functionType tv tv, t1)])

  ExpressionFunction f -> case f of
--    FunctionCases cases -> TODO
--    FunctionCompareTo other -> TODO

    FunctionLambda (Lambda x e) -> do
      tv <- fresh
      (t, c) <- inGamma (x, TypeScheme [] tv) (infer e)
      return (functionType tv t, c)

--    FunctionPrimitive name -> TODO
--    FunctionProjection fname -> TODO

  ExpressionIf (If cond tr fl) -> do
    (t1, c1) <- infer cond
    (t2, c2) <- infer tr
    (t3, c3) <- infer fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, booleanType), (t2, t3)])

  ExpressionLet (Let x e1 e2) -> do
    env <- ask
    (t1, c1) <- infer e1
    case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (substMap substTypeScheme sub env) (substType sub t1)
            (t2, c2) <- inGamma (x, sc) $ local (substMap substTypeScheme sub) (infer e2)
            return (t2, c1 ++ c2)

--  ExpressionList els -> TODO

  ExpressionLiteral l -> return (TypeLiteral $ literalType l, [])

--  ExpressionMap m -> TODO

  ExpressionOp (Op op e1 e2) -> do
    (t1, c1) <- infer e1
    (t2, c2) <- infer e2
    tv <- fresh
    let u1 = functionType t1 (functionType t2 tv)
        u2 = binopType op
    return (tv, c1 ++ c2 ++ [(u1, u2)])

--  ExpressionOptional m -> TODO
--  ExpressionRecord fields -> TODO
--  ExpressionSet els -> TODO
--  ExpressionUnion field -> TODO

  ExpressionVariable x -> do
      t <- lookupGamma x
      return (t, [])

inferTop :: Gamma -> [(String, Term a)] -> Either TypeError Gamma
inferTop env [] = Right env
inferTop env ((name, ex):xs) = case inferExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (M.insert name ty env) xs

inferType :: Term a -> Result TypeScheme
inferType term = case inferTop M.empty [("x", term)] of
  Left err -> fail $ "type inference failed: " ++ show err
  Right m -> case M.lookup "x" m of
    Nothing -> fail "inferred type not resolved"
    Just scheme -> pure scheme

normalizeTypeScheme :: TypeScheme -> TypeScheme
normalizeTypeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normtype body)
  where
    ord = zip (L.nub $ fv body) variables

    fv (TypeVariable a)   = [a]
    fv (TypeFunction (FunctionType a b)) = fv a ++ fv b
    fv (TypeLiteral _)    = []

    normtype (TypeFunction (FunctionType a b)) = functionType (normtype a) (normtype b)
    normtype (TypeLiteral a)   = TypeLiteral a
    normtype (TypeVariable a)   =
      case Prelude.lookup a ord of
        Just x -> TypeVariable x
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | Compose substitutions
substCompose :: Subst -> Subst -> Subst
substCompose s1 s2 = M.union s1 $ M.map (substType s1) s2

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver (M.empty, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (substList substType su1 ts1) (substList substType su1 ts2)
     return (substCompose su2 su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

unifies :: Type -> Type -> Solve Subst
unifies t1 t2 | t1 == t2 = return M.empty
unifies (TypeVariable v) t = bind v t
unifies t (TypeVariable v) = bind v t
unifies (TypeFunction (FunctionType t1 t2)) (TypeFunction (FunctionType t3 t4)) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ UnificationFail t1 t2

-- Unification solver
solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
      su1  <- unifies t1 t2
      solver (substCompose su1 su, substList substConstraint su1 cs0)

bind ::  TypeVariable -> Type -> Solve Subst
bind a t | t == TypeVariable a = return M.empty
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

occursCheck ::  TypeVariable -> Type -> Bool
occursCheck a t = S.member a $ freeVarsInType t
