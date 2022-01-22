module Hydra.Prototyping.Types.Inference (
  inferType,
  Constraint,
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

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: Show m => Context m -> Term m -> Result Type
decodeStructuralType cx term = do
  typ <- decodeType cx term
  case typ of
    TypeNominal name -> do
      scx <- schemaContext cx
      el <- requireElement scx name
      decodeStructuralType scx $ elementData el
    _ -> pure typ

freshTypeVariable :: Infer Type
freshTypeVariable = do
    s <- get
    put (s + 1)
    return $ TypeVariable (normalVariables !! s)

generalize :: TypingEnvironment -> Type -> TypeScheme
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

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
      ExpressionApplication (Application fun arg) -> do
        ifun <- infer cx fun
        iarg <- infer cx arg
        v <- freshTypeVariable
        let c = (termConstraints ifun) ++ (termConstraints iarg) ++ [(termType ifun, functionType (termType iarg) v)]
        let app = ExpressionApplication $ Application ifun iarg
        yield app v c

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
                _ -> error $ "expected a function type in case " ++ show (fieldName field) ++ "; found " ++ show ft
                  ++ " when inferring type of function " ++ show f
--                _ -> error $ "expected a function type in case " ++ show (fieldName field) ++ "; found " ++ show field1

        -- Note: here we assume that compareTo evaluates to an integer, not a Comparison value.
        --       For the latter, Comparison would have to be added to the literal type grammar.
        FunctionCompareTo other -> do
          i <- infer cx other
          yieldFunction (FunctionCompareTo i) (functionType (termType i) int8Type) (termConstraints i)

        FunctionData -> do
          et <- freshTypeVariable
          yieldFunction FunctionData (functionType (elementType et) et) []

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
        case solveConstraints cx c1 of
            Left err -> throwError err
            Right sub -> do
                let sc = generalize (M.map (substituteInScheme sub) env) (substituteInType sub t1)
                i2 <- extendEnvironment (x, sc) $ local (M.map (substituteInScheme sub)) (infer cx e2)
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
  => TypingEnvironment -> Context m -> TypeVariable -> Term m
  -> Either TypeError (Term (m, Type, [Constraint]), TypingEnvironment)
inferTop env cx v term = do
    term1 <- runInference env (infer cx term)
    let (ResultSuccess scon) = schemaContext cx
    subst <- solveConstraints scon (termConstraints term1)
    let replace typ = substituteInType subst typ
    let term2 = rewriteTermType replace term1
    let ts = closeOver $ termType term2
    return (term2, M.insert v ts env)
  where
    -- | Canonicalize and return the polymorphic toplevel type.
    closeOver :: Type -> TypeScheme
    closeOver = normalizeScheme . generalize M.empty

inferType :: (Default m, Ord m, Show m) => Context m -> Term m -> Result (Term (m, Type, [Constraint]), TypeScheme)
inferType cx term = case inferTop M.empty cx var term of
    Left err -> fail $ "type inference failed: " ++ show err
    Right (term1, env) -> case M.lookup var env of
      Nothing -> fail "inferred type not resolved"
      Just ts -> pure (term1, ts)
  where
    var = "x"

instantiate ::  TypeScheme -> Infer Type
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshTypeVariable) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

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

rewriteTermType :: (Type -> Type) -> Term (m, Type, [Constraint]) -> Term (m, Type, [Constraint])
rewriteTermType f (Term expr (x, typ, c)) = Term expr (x, f typ, c) -- TODO: replace recursively

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
