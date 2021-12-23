module Hydra.Prototyping.Types.Inference (
  inferType,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Basics
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreDecoding
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
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
import qualified Data.Maybe as Y


type Infer a = ReaderT TypingEnvironment (StateT InferenceState (Except TypeError)) a

type InferenceState = Int

type TypingEnvironment = M.Map TypeVariable TypeScheme

type Unifier = (Subst, [Constraint])

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: Show m => Context m -> Term m -> Result Type
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

extendEnvironment :: (Variable, TypeScheme) -> Infer a -> Infer a
extendEnvironment (x, sc) m = do
  let scope e = M.insert x sc $ M.delete x e
  local scope m

infer :: (Default m, Ord m, Show m) => Context m -> Term m -> Infer (Term (m, Type, [Constraint]))
infer cx term = case contextTypeOf cx (termMeta term) of
    Just typ -> do
      i <- inferInternal
      return i { termMeta = (termMeta term, typ, [])} -- TODO: unify "suggested" types with inferred types
    Nothing -> inferInternal
  where
    yield expr typ constraints = return Term {
      termData = expr,
      termMeta = (termMeta term, typ, constraints)}

    yieldFunction fun = yield (ExpressionFunction fun)

    inferInternal = case termData term of
      ExpressionApplication (Application e1 e2) -> do
        i1 <- infer cx e1
        i2 <- infer cx e2
        let t1 = termType i1
        let t2 = termType i2
        let c1 = termConstraints i1
        let c2 = termConstraints i2
        let e = ExpressionApplication $ Application i1 i2
        t <- freshTypeVariable
        let c = c1 ++ c2 ++ [(t1, functionType t2 t)]
        yield e t c

      ExpressionElement name -> do
        case typeOfElement cx name of
          -- TODO: polytyped elements will probably be allowed in the future
          ResultSuccess et -> yield (ExpressionElement name) (elementType et) []
          ResultFailure msg -> error msg

      ExpressionFunction f -> case f of
        FunctionCases cases -> do
            pairs <- CM.mapM forField cases
            let icases = fst . snd <$> pairs
            let cods = snd . fst <$> pairs
            let ftypes = L.zipWith FieldType (fieldName <$> cases) (fst . fst <$> pairs)
            let c1 = L.concat (snd . snd <$> pairs)
            let c2 = L.zip cods (L.tail cods)
            yieldFunction (FunctionCases icases) (functionType (unionType ftypes) (L.head cods)) (c1 ++ c2)
          where
            forField field = do
              field1 <- inferFieldType cx field
              let ft = termType $ fieldTerm field1
              let c = termConstraints $ fieldTerm field1
              case ft of
                TypeFunction (FunctionType dom cod) -> return ((dom, cod), (field1, c))
                _ -> error "expected a function type"

        -- Note: here we assume that compareTo evaluates to an integer, not a Comparison value.
        --       For the latter, Comparison would have to be added to the literal type grammar.
        FunctionCompareTo other -> do
          i <- infer cx other
          yieldFunction (FunctionCompareTo i) (functionType (termType i) int8Type) (termConstraints i)

        FunctionLambda (Lambda x e) -> do
          tv <- freshTypeVariable
          i <- extendEnvironment (x, TypeScheme [] tv) (infer cx e)
          yieldFunction (FunctionLambda $ Lambda x i) (functionType tv (termType i)) (termConstraints i) -- TODO: is x constant?

        FunctionPrimitive name -> do
          case typeOfPrimitiveFunction cx name of
            ResultSuccess t -> yieldFunction (FunctionPrimitive name) (TypeFunction t) [] -- TODO: account for polytyped primitive functions
            ResultFailure msg -> error msg

        -- Note: type inference cannot recover complete record types from projections; type annotations are needed
        FunctionProjection fname -> do
          dom <- freshTypeVariable
          cod <- freshTypeVariable
          let ftype = functionType (TypeRecord [FieldType fname dom]) cod
          yieldFunction (FunctionProjection fname) ftype []

        _ -> error $ "type inference is unsupported for function: " ++ show f

      ExpressionLet (Let x e1 e2) -> do
        env <- ask
        i1 <- infer cx e1
        let t1 = termType i1
        let c1 = termConstraints i1
        case solveConstraints c1 of
            Left err -> throwError err
            Right sub -> do
                let sc = generalize (M.map (sustituteVariablesInTypeScheme sub) env) (sustituteVariablesInType sub t1)
                i2 <- extendEnvironment (x, sc) $ local (M.map (sustituteVariablesInTypeScheme sub)) (infer cx e2)
                let t2 = termType i2
                let c2 = termConstraints i2
                yield (ExpressionLet $ Let x i1 i2) t2 (c1 ++ c2) -- TODO: is x constant?

      ExpressionList els -> do
          (els1, t, c) <- forList els
          yield (ExpressionList els1) t c
        where
          forList l = case l of
            [] -> do
              v <- freshTypeVariable
              return ([], listType v, [])
            (h:r) -> do
              i <- infer cx h
              let t = termType i
              let c = termConstraints i
              (ir, lt, lc) <- forList r
              let (TypeList et) = lt
              return (i:ir, lt ,c ++ lc ++ [(t, et)])

      ExpressionLiteral l -> yield (ExpressionLiteral l) (TypeLiteral $ literalType l) []

      ExpressionMap m -> do
          (l, kt, vt, c) <- forList (M.toList m)
          yield (ExpressionMap $ M.fromList l) (mapType kt vt) c
        where
          forList l = case l of
            [] -> do
              kv <- freshTypeVariable
              vv <- freshTypeVariable
              return ([], kv, vv, [])
            ((k, v):r) -> do
              ki <- infer cx k
              let kt = termType ki
              let kc = termConstraints ki
              vi <- infer cx v
              let vt = termType vi
              let vc = termConstraints vi
              (r, kt1, vt1, c1) <- forList r
              return ((ki, vi):r, kt, vt, c1 ++ kc ++ vc ++ [(kt, kt1), (vt, vt1)])

      ExpressionNominal (NominalTerm name term1) -> do
        case namedType cx name of
          ResultFailure msg -> error msg
          ResultSuccess typ -> do
            i <- infer cx term1
            let typ1 = termType i
            let c = termConstraints i
            yield (ExpressionNominal $ NominalTerm name i) typ (c ++ [(typ, typ1)])

      ExpressionOptional m -> case m of
        Nothing -> do
          tv <- freshTypeVariable
          yield (ExpressionOptional Nothing) (optionalType tv) []
        Just term1 -> do
          i <- infer cx term1
          yield (ExpressionOptional $ Just i) (optionalType (termType i)) (termConstraints i)

      ExpressionRecord fields -> do
          (fields0, ftypes0, c1) <- CM.foldM forField ([], [], []) fields
          yield (ExpressionRecord $ L.reverse fields0) (recordType $ L.reverse ftypes0) c1
        where
          forField (typed, ftypes, c) field = do
            i <- inferFieldType cx field
            let ft = termType $ fieldTerm i
            let c1 = termConstraints $ fieldTerm i
            return (i:typed, (FieldType (fieldName field) ft):ftypes, c1 ++ c)

      ExpressionSet els -> do
        let expr = ExpressionList $ S.toList els
        i <- infer cx $ term {termData = expr}
        let (TypeList et) = termType i
        let (ExpressionList els1) = termData i
        yield (ExpressionSet $ S.fromList els1) (TypeSet et) (termConstraints i)

      -- Note: type inference cannot recover complete union types from union values; type annotations are needed
      ExpressionUnion field -> do
        ifield <- inferFieldType cx field
        let typ = TypeUnion [FieldType (fieldName field) (termType $ fieldTerm ifield)]
        yield (ExpressionUnion ifield) typ (termConstraints $ fieldTerm ifield)

      ExpressionVariable x -> do
        t <- lookupTypeInEnvironment x
        yield (ExpressionVariable x) t []

      _ -> error $ "type inference is unsupported for term: " ++ show term

inferFieldType :: (Default m, Ord m, Show m) => Context m -> Field m -> Infer (Field (m, Type, [Constraint]))
inferFieldType cx (Field fname term) = Field fname <$> infer cx term

-- | Solve for the toplevel type of an expression in a given environment
inferTop :: (Default m, Ord m, Show m)
  => TypingEnvironment -> Context m -> TypeVariable -> Term m -> Either TypeError TypingEnvironment
inferTop env cx v term = do
    term1 <- runInference env (infer cx term)
    subst <- solveConstraints (termConstraints term1)
    let replace typ = sustituteVariablesInType subst typ
    let term2 = replaceTermType replace term1 -- TODO: use me
    let ts = closeOver $ termType term2
    return $ M.insert v ts env
  where
    -- | Canonicalize and return the polymorphic toplevel type.
    closeOver :: Type -> TypeScheme
    closeOver = normalizeTypeScheme . generalize M.empty

inferType :: (Default m, Ord m, Show m) => Context m -> Term m -> Result TypeScheme --Result (Term (m, Type), TypeScheme)
inferType cx term = case inferTop M.empty cx "x" term of
  Left err -> fail $ "type inference failed: " ++ show err
  Right env -> case M.lookup "x" env of
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

namedType :: Show m => Context m -> Name -> Result Type
namedType cx name = do
  scon <- schemaContext cx
  el <- requireElement scon name
  scon' <- schemaContext scon
  decodeStructuralType scon' $ elementData el

replaceTermType :: (Type -> Type) -> Term (m, Type, [Constraint]) -> Term (m, Type, [Constraint])
replaceTermType f (Term expr (x, typ, c)) = Term expr (x, f typ, c) -- TODO: replace recursively

solveConstraints :: [Constraint] -> Either TypeError Subst
solveConstraints cs = runIdentity $ runExceptT $ unificationSolver (M.empty, cs)

runInference :: TypingEnvironment -> Infer (Term (m, Type, [Constraint])) -> Either TypeError (Term (m, Type, [Constraint]))
runInference env term = runExcept $ evalStateT (runReaderT term env) startState

startState :: InferenceState
startState = 0

termConstraints :: Term (m, Type, [Constraint]) -> [Constraint]
termConstraints (Term _ (_, _, constraints)) = constraints

termType :: Term (m, Type, [Constraint]) -> Type
termType (Term _ (_, typ, _)) = typ

typeOfElement :: Show m => Context m -> Name -> Result Type
typeOfElement cx name = do
  el <- requireElement cx name
  decodeStructuralType cx $ elementSchema el

typeOfPrimitiveFunction :: Context m -> Name -> Result FunctionType
typeOfPrimitiveFunction cx name = primitiveFunctionType <$> requirePrimitiveFunction cx name

unificationSolver :: Unifier -> Solve Subst
unificationSolver (su, cs) = case cs of
  [] -> return su
  ((t1, t2): cs0) -> do
    su1  <- unify t1 t2
    unificationSolver (
      composeSubstitutions su1 su,
      (\(t1, t2) -> (sustituteVariablesInType su1 t1, sustituteVariablesInType su1 t2)) <$> cs0)
