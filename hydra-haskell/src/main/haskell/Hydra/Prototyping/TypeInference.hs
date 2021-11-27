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

module Hydra.Prototyping.TypeInference (
  Constraint,
  TypeError(..),
  inferType,
  constraintsExpr,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding
import Hydra.Impl.Haskell.Dsl

import qualified Control.Monad as CM
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type Infer a = ReaderT TypingEnvironment (StateT InferenceState (Except TypeError)) a

type InferenceState = Int

type Constraint = (Type, Type)

type Unifier = (Subst, [Constraint])

type Solve a = ExceptT TypeError Identity a

type TypingEnvironment = M.Map Variable TypeScheme

type Subst = M.Map TypeVariable Type


startState :: InferenceState
startState = 0

substInType :: M.Map TypeVariable Type -> Type -> Type
substInType s typ = case typ of
    TypeElement t -> elementType $ subst t
    TypeFunction (FunctionType dom cod) -> functionType (subst dom) (subst cod)
    TypeList t -> listType $ subst t
    TypeLiteral lt -> typ
    TypeMap (MapType kt vt) -> mapType (subst kt) (subst vt)
    TypeNominal _ -> typ -- because we do not allow names to be bound to types with free variables
    TypeOptional t -> optionalType $ subst t
    TypeRecord tfields -> recordType (substField <$> tfields)
    TypeSet t -> setType $ subst t
    TypeUnion tfields -> unionType (substField <$> tfields)
    TypeVariable a -> M.findWithDefault typ a s
  where
    subst = substInType s
    substField (FieldType fname t) = FieldType fname $ subst t

freeVarsInType :: Type -> S.Set TypeVariable
freeVarsInType typ = S.fromList $ fv typ
  where
    fv typ = case typ of
      TypeElement t -> fv t
      TypeFunction (FunctionType dom cod) -> fv dom ++ fv cod
      TypeList t -> fv t
      TypeLiteral _ -> []
      TypeMap (MapType kt vt) -> fv kt ++ fv vt
      TypeNominal _ -> [] -- because we do not allow names to be bound to types with free variables
      TypeOptional t -> fv t
      TypeRecord tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeSet t -> fv t
      TypeUnion tfields -> L.concat (fv . fieldTypeType <$> tfields)
      TypeVariable v -> [v]

--instance Substitutable TypeScheme where
substInTypeScheme :: M.Map TypeVariable Type -> TypeScheme -> TypeScheme
substInTypeScheme s (TypeScheme as t) = TypeScheme as $ substInType s' t
  where
    s' = L.foldr M.delete s as

freeVarsInTypeScheme :: TypeScheme -> S.Set TypeVariable
freeVarsInTypeScheme (TypeScheme as t) = S.difference (freeVarsInType t) (S.fromList as)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVariable Type
  | UnboundVariable String
  | UnificationMismatch [Type] [Type] deriving Show

-- | Run the inference monad
runInfer :: TypingEnvironment -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runReaderT m env) startState

-- | Solve for the toplevel type of an expression in a given environment
inferExpr :: (Default a, Show a) => TypingEnvironment -> Context a -> Term a -> Either TypeError TypeScheme
inferExpr env context ex = case runInfer env (infer context ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ substInType subst ty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: (Default a, Show a) => TypingEnvironment -> Context a -> Term a -> Either TypeError ([Constraint], Subst, Type, TypeScheme)
constraintsExpr env context ex = case runInfer env (infer context ex) of
  Left err -> Left err
  Right (ty, cs) -> case runSolve cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ substInType subst ty

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TypeScheme
closeOver = normalizeTypeScheme . generalize M.empty

-- | Extend type environment
inTypingEnvironment :: (Variable, TypeScheme) -> Infer a -> Infer a
inTypingEnvironment (x, sc) m = do
  let scope e = M.insert x sc $ M.delete x e
  local scope m

-- | Lookup type in the environment
lookupTypingEnvironment :: Variable -> Infer Type
lookupTypingEnvironment v = do
  env <- ask
  case M.lookup v env of
      Nothing   -> throwError $ UnboundVariable v
      Just s    -> instantiate s

variables :: [String]
variables = (\n -> "v" ++ show n) <$> [1..]

freshTypeVariable :: Infer Type
freshTypeVariable = do
    s <- get
    put (s + 1)
    return $ TypeVariable (variables !! s)

instantiate ::  TypeScheme -> Infer Type
instantiate (TypeScheme as t) = do
    as' <- mapM (const freshTypeVariable) as
    let s = M.fromList $ zip as as'
    return $ substInType s t

generalize :: TypingEnvironment -> Type -> TypeScheme
generalize env t  = TypeScheme as t
  where
    as = S.toList $ S.difference (freeVarsInType t) (L.foldr (S.union . freeVarsInTypeScheme) S.empty $ M.elems env)

binopType :: Binop -> Type
binopType BinopAdd = functionType int32Type (functionType int32Type int32Type)
binopType BinopMul = functionType int32Type (functionType int32Type int32Type)
binopType BinopSub = functionType int32Type (functionType int32Type int32Type)
binopType BinopEql = functionType int32Type (functionType int32Type booleanType)

typeOfElement :: Show a => Context a -> Name -> Result Type
typeOfElement context name = do
  el <- requireElement context name
  scon <- schemaContext context
  decodeType scon $ elementSchema el

typeOfPrimitiveFunction :: Context a -> Name -> Result FunctionType
typeOfPrimitiveFunction context name = primitiveFunctionType <$> requirePrimitiveFunction context name

infer :: (Default a, Show a) => Context a -> Term a -> Infer (Type, [Constraint])
infer context term = case termData term of

  ExpressionApplication (Application e1 e2) -> do
    (t1, c1) <- infer context e1
    (t2, c2) <- infer context e2
    tv <- freshTypeVariable
    return (tv, c1 ++ c2 ++ [(t1, functionType t2 tv)])

  ExpressionElement name -> do
    case typeOfElement context name of
      ResultSuccess et -> pure (elementType et, []) -- TODO: polytyped elements may be allowed in the future
      ResultFailure msg -> error msg

  ExpressionFix e1 -> do
    (t1, c1) <- infer context e1
    tv <- freshTypeVariable
    return (tv, c1 ++ [(functionType tv tv, t1)])

  ExpressionFunction f -> case f of
--    FunctionCases cases -> TODO

    FunctionCompareTo other -> do
      (t, c) <- infer context other
      return (functionType t booleanType, c)

    FunctionLambda (Lambda x e) -> do
      tv <- freshTypeVariable
      (t, c) <- inTypingEnvironment (x, TypeScheme [] tv) (infer context e)
      return (functionType tv t, c)

    FunctionPrimitive name -> do
      case typeOfPrimitiveFunction context name of
        ResultSuccess t -> pure (TypeFunction t, []) -- TODO: polytyped primitive functions may be allowed in the future
        ResultFailure msg -> error msg

--    FunctionProjection fname -> TODO

  ExpressionIf (If cond tr fl) -> do
    (t1, c1) <- infer context cond
    (t2, c2) <- infer context tr
    (t3, c3) <- infer context fl
    return (t2, c1 ++ c2 ++ c3 ++ [(t1, booleanType), (t2, t3)])

  ExpressionLet (Let x e1 e2) -> do
    env <- ask
    (t1, c1) <- infer context e1
    case runSolve c1 of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (M.map (substInTypeScheme sub) env) (substInType sub t1)
            (t2, c2) <- inTypingEnvironment (x, sc) $ local (M.map (substInTypeScheme sub)) (infer context e2)
            return (t2, c1 ++ c2)

--  ExpressionList els -> TODO

  ExpressionLiteral l -> return (TypeLiteral $ literalType l, [])

--  ExpressionMap m -> TODO

  ExpressionOp (Op op e1 e2) -> do
    (t1, c1) <- infer context e1
    (t2, c2) <- infer context e2
    tv <- freshTypeVariable
    let u1 = functionType t1 (functionType t2 tv)
        u2 = binopType op
    return (tv, c1 ++ c2 ++ [(u1, u2)])

  ExpressionOptional m -> case m of
    Nothing -> do
      tv <- freshTypeVariable
      return (optionalType tv, [])
    Just term' -> do
      (t, c) <- infer context term'
      return (optionalType t, c)

  ExpressionRecord fields -> do
      (ftypes, c1) <- CM.foldM forField ([], []) fields
      return (recordType $ L.reverse ftypes, c1)
    where
      forField (ftypes, c) field = do
        (ft, c') <- inferFieldType context field
        return (ft:ftypes, c' ++ c)

--  ExpressionSet els -> TODO

  ExpressionUnion field -> do
    (ft, c1) <- inferFieldType context field
    return (unionType [ft], c1)

  ExpressionVariable x -> do
      t <- lookupTypingEnvironment x
      return (t, [])

inferFieldType :: (Default a, Show a) => Context a -> Field a -> Infer (FieldType, [Constraint])
inferFieldType context (Field fname term) = do
  (t1, c1) <- infer context term
  return (FieldType fname t1, c1)

inferTop :: (Default a, Show a) => TypingEnvironment -> Context a -> [(String, Term a)] -> Either TypeError TypingEnvironment
inferTop env _ [] = Right env
inferTop env context ((name, ex):xs) = case inferExpr env context ex of
  Left err -> Left err
  Right ty -> inferTop (M.insert name ty env) context xs

inferType :: (Default a, Show a) => Context a -> Term a -> Result TypeScheme
inferType context term = case inferTop M.empty context [("x", term)] of
  Left err -> fail $ "type inference failed: " ++ show err
  Right m -> case M.lookup "x" m of
    Nothing -> fail "inferred type not resolved"
    Just scheme -> pure scheme

normalizeTypeScheme :: TypeScheme -> TypeScheme
normalizeTypeScheme (TypeScheme _ body) = TypeScheme (fmap snd ord) (normalizeType body)
  where
    ord = L.zip (S.toList $ freeVarsInType body) variables

    normalizeFieldType (FieldType fname typ) = FieldType fname $ normalizeType typ

    normalizeType typ = case typ of
      TypeElement t -> TypeElement $ normalizeType t
      TypeFunction (FunctionType dom cod) -> functionType (normalizeType dom) (normalizeType cod)
      TypeList t -> TypeList $ normalizeType t
      TypeLiteral l -> typ
      TypeMap (MapType kt vt) -> TypeMap $ MapType (normalizeType kt) (normalizeType vt)
      TypeNominal _ -> typ
      TypeOptional t -> TypeOptional $ normalizeType t
      TypeRecord fields -> TypeRecord (normalizeFieldType <$> fields)
      TypeSet t -> TypeSet $ normalizeType t
      TypeUnion fields -> TypeUnion (normalizeFieldType <$> fields)
      TypeUniversal (UniversalType v t) -> TypeUniversal $ UniversalType v $ normalizeType t
      TypeVariable v -> case Prelude.lookup v ord of
        Just v' -> TypeVariable v'
        Nothing -> error "type variable not in signature"

-------------------------------------------------------------------------------
-- Constraint Solver
-------------------------------------------------------------------------------

-- | Compose substitutions
substCompose :: Subst -> Subst -> Subst
substCompose s1 s2 = M.union s1 $ M.map (substInType s1) s2

-- | Run the constraint solver
runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runIdentity $ runExceptT $ solver (M.empty, cs)

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return M.empty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (substInType su1 <$> ts1) (substInType su1 <$> ts2)
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
solver (su, cs) = case cs of
  [] -> return su
  ((t1, t2): cs0) -> do
    su1  <- unifies t1 t2
    solver (substCompose su1 su, (\(t1, t2) -> (substInType su1 t1, substInType su1 t2)) <$> cs0)

bind ::  TypeVariable -> Type -> Solve Subst
bind a t | t == TypeVariable a = return M.empty
         | variableOccursInType a t = throwError $ InfiniteType a t
         | otherwise = return $ M.singleton a t

variableOccursInType ::  TypeVariable -> Type -> Bool
variableOccursInType a t = S.member a $ freeVarsInType t
