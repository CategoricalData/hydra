module Hydra.Prototyping.Types.Inference (
  inferType,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Prototyping.Types.Substitution
import Hydra.Prototyping.Types.Unification

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

type TypingEnvironment = M.Map Variable TypeScheme

type Unifier = (Subst, [Constraint])

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> TypeScheme
closeOver = normalizeTypeScheme . generalize M.empty

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: (Default a, Show a) => TypingEnvironment -> Context a -> Term a -> Either TypeError ([Constraint], Subst, Type, TypeScheme)
constraintsExpr env context ex = case runInference env (infer context ex) of
  Left err -> Left err
  Right (ty, cs) -> case solveConstraints cs of
    Left err -> Left err
    Right subst -> Right (cs, subst, ty, sc)
      where
        sc = closeOver $ sustituteVariablesInType subst ty

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: Show a => Context a -> Term a -> Result Type
decodeStructuralType con term = do
  typ <- decodeType con term
  case typ of
    TypeNominal name -> do
      scon <- schemaContext con
      el <- requireElement scon name
      decodeStructuralType scon $ elementData el
    _ -> pure typ

freshTypeVariable :: Infer Type
freshTypeVariable = do
    s <- get
    put (s + 1)
    return $ TypeVariable (normalVariables !! s)

generalize :: TypingEnvironment -> Type -> TypeScheme
generalize env t  = TypeScheme as t
  where
    as = S.toList $ S.difference (freeVariablesInType t) (L.foldr (S.union . freeVariablesInTypeScheme) S.empty $ M.elems env)

-- | Extend type environment
inTypingEnvironment :: (Variable, TypeScheme) -> Infer a -> Infer a
inTypingEnvironment (x, sc) m = do
  let scope e = M.insert x sc $ M.delete x e
  local scope m

infer :: (Default a, Show a) => Context a -> Term a -> Infer (Type, [Constraint])
infer context term = case termData term of

  ExpressionApplication (Application e1 e2) -> do
    (t1, c1) <- infer context e1
    (t2, c2) <- infer context e2
    tv <- freshTypeVariable
    return (tv, c1 ++ c2 ++ [(t1, functionType t2 tv)])

  ExpressionElement name -> do
    case typeOfElement context name of
      ResultSuccess et -> pure (elementType et, []) -- TODO: polytyped elements will probably be allowed in the future
      ResultFailure msg -> error msg

  ExpressionFunction f -> case f of
    FunctionCases cases -> do
        pairs <- CM.mapM forField cases
        let cods = snd <$> pairs
        let ftypes = L.zipWith FieldType (fieldName <$> cases) (fst <$> pairs)
        let constraints = L.zip cods (L.tail cods)
        return (functionType (unionType ftypes) (L.head cods), constraints)
      where
        forField (Field _ fun) = do
          (ft, c) <- infer context fun
          case ft of
            TypeFunction (FunctionType dom cod) -> return (dom, cod)
            _ -> error "expected a function type"

    -- TODO: here we assume that compareTo evaluates to an integer, not a Comparison value.
    --       For the latter, Comparison would have to be added to the literal type grammar.
    FunctionCompareTo other -> do
      (t, c) <- infer context other
      return (functionType t int8Type, c)

    FunctionLambda (Lambda x e) -> do
      tv <- freshTypeVariable
      (t, c) <- inTypingEnvironment (x, TypeScheme [] tv) (infer context e)
      return (functionType tv t, c)

    FunctionPrimitive name -> do
      case typeOfPrimitiveFunction context name of
        ResultSuccess t -> pure (TypeFunction t, []) -- TODO: polytyped primitive functions may be allowed in the future
        ResultFailure msg -> error msg

    FunctionProjection (Projection fname sname) -> do
      case namedType context sname of
        ResultFailure msg -> error msg
        ResultSuccess typ -> do
          case typ of
            TypeRecord sfields -> if L.null matches
                then error $ "no field " ++ show fname ++ " found in record type " ++ show sname
                else return (functionType typ (fieldTypeType $ L.head matches), [])
              where
                matches = L.filter (\f -> fieldTypeName f == fname) sfields
            _ -> error $ "type name " ++ show sname ++ " did not resolve to a record type"

  ExpressionLet (Let x e1 e2) -> do
    env <- ask
    (t1, c1) <- infer context e1
    case solveConstraints c1 of
        Left err -> throwError err
        Right sub -> do
            let sc = generalize (M.map (sustituteVariablesInTypeScheme sub) env) (sustituteVariablesInType sub t1)
            (t2, c2) <- inTypingEnvironment (x, sc) $ local (M.map (sustituteVariablesInTypeScheme sub)) (infer context e2)
            return (t2, c1 ++ c2)

  ExpressionList els -> forList els
    where
      forList l = case l of
        [] -> do
          tv <- freshTypeVariable
          return (listType tv, [])
        (h:r) -> do
          (t, c) <- infer context h
          (lt, lc) <- forList r
          case lt of
            TypeList et -> return (lt, c ++ lc ++ [(t, et)])
            _ -> error "expected a list type"

  ExpressionLiteral l -> return (TypeLiteral $ literalType l, [])

  ExpressionMap m -> toMap <$> forList (M.toList m)
    where
      toMap ((kt, vt), c) = (mapType kt vt, c)
      forList l = case l of
        [] -> do
          kv <- freshTypeVariable
          vv <- freshTypeVariable
          return ((kv, vv), [])
        ((k, v):r) -> do
          (kt, kc) <- infer context k
          (vt, vc) <- infer context v
          ((kt', vt'), c') <- forList r
          return ((kt, vt), c' ++ kc ++ vc ++ [(kt, kt'), (vt, vt')])

  ExpressionNominal (NominalTerm name term') -> do
    case namedType context name of
      ResultFailure msg -> error msg
      ResultSuccess typ -> do
        (typ', c) <- infer context term'
        return (typ, c ++ [(typ, typ')])

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

  ExpressionSet els -> do
    let expr = ExpressionList $ S.toList els
    (t, c) <- infer context $ term {termData = expr}
    case t of
      TypeList et -> return (TypeSet et, c)
      _ -> error "expected a list type"

  ExpressionUnion (UnionExpression sname (Field fname fterm)) -> case namedType context sname of
    ResultFailure msg -> error msg
    ResultSuccess typ -> do
      (ftype, _) <- infer context fterm
      case typ of
        TypeUnion sfields -> if L.null matches
            then error $ "no field " ++ show fname ++ " found in union type " ++ show sname
            else return (typ, [(ftype, fieldTypeType (L.head matches))])
          where
            matches = L.filter (\f -> fieldTypeName f == fname) sfields
        _ -> error $ "type name " ++ show sname ++ " did not resolve to a union type"

  ExpressionVariable x -> do
      t <- lookupTypeInEnvironment x
      return (t, [])

-- | Solve for the toplevel type of an expression in a given environment
inferTerm :: (Default a, Show a) => TypingEnvironment -> Context a -> Term a -> Either TypeError TypeScheme
inferTerm env context ex = case runInference env (infer context ex) of
  Left err -> Left err
  Right (ty, cs) -> case solveConstraints cs of
    Left err -> Left err
    Right subst -> Right $ closeOver $ sustituteVariablesInType subst ty

inferFieldType :: (Default a, Show a) => Context a -> Field a -> Infer (FieldType, [Constraint])
inferFieldType context (Field fname term) = do
  (t1, c1) <- infer context term
  return (FieldType fname t1, c1)

inferTop :: (Default a, Show a) => TypingEnvironment -> Context a -> [(String, Term a)] -> Either TypeError TypingEnvironment
inferTop env _ [] = Right env
inferTop env context ((name, ex):xs) = case inferTerm env context ex of
  Left err -> Left err
  Right ty -> inferTop (M.insert name ty env) context xs

inferType :: (Default a, Show a) => Context a -> Term a -> Result TypeScheme
inferType context term = case inferTop M.empty context [("x", term)] of
  Left err -> fail $ "type inference failed: " ++ show err
  Right m -> case M.lookup "x" m of
    Nothing -> fail "inferred type not resolved"
    Just scheme -> pure scheme

instantiate ::  TypeScheme -> Infer Type
instantiate (TypeScheme as t) = do
    as' <- mapM (const freshTypeVariable) as
    let s = M.fromList $ zip as as'
    return $ sustituteVariablesInType s t

lookupTypeInEnvironment :: Variable -> Infer Type
lookupTypeInEnvironment v = do
  env <- ask
  case M.lookup v env of
      Nothing   -> throwError $ UnboundVariable v
      Just s    -> instantiate s

namedType :: Show a => Context a -> Name -> Result Type
namedType context name = do
  scon <- schemaContext context
  el <- requireElement scon name
  scon' <- schemaContext scon
  decodeStructuralType scon' $ elementData el

solveConstraints :: [Constraint] -> Either TypeError Subst
solveConstraints cs = runIdentity $ runExceptT $ unificationSolver (M.empty, cs)

runInference :: TypingEnvironment -> Infer (Type, [Constraint]) -> Either TypeError (Type, [Constraint])
runInference env m = runExcept $ evalStateT (runReaderT m env) startState

startState :: InferenceState
startState = 0

typeOfElement :: Show a => Context a -> Name -> Result Type
typeOfElement context name = do
  el <- requireElement context name
  decodeStructuralType context $ elementSchema el

typeOfPrimitiveFunction :: Context a -> Name -> Result FunctionType
typeOfPrimitiveFunction context name = primitiveFunctionType <$> requirePrimitiveFunction context name

unificationSolver :: Unifier -> Solve Subst
unificationSolver (su, cs) = case cs of
  [] -> return su
  ((t1, t2): cs0) -> do
    su1  <- unify t1 t2
    unificationSolver (
      composeSubstitutions su1 su,
      (\(t1, t2) -> (sustituteVariablesInType su1 t1, sustituteVariablesInType su1 t2)) <$> cs0)
