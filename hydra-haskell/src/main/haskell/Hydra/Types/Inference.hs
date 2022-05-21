module Hydra.Types.Inference (
  inferType,
  Constraint,
) where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Basics
import Hydra.Primitives
import Hydra.CoreDecoding
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Extras
import Hydra.Types.Substitution
import Hydra.Types.Unification
import Hydra.Rewriting

import qualified Control.Monad as CM
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


type Infer a m = ReaderT (TypingEnvironment m) (StateT InferenceState (Except (TypeError m))) a

type InferenceState = Int

type TypingEnvironment m = M.Map Variable (TypeScheme m)

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: (Default m, Show m) => Context m -> Data m -> Result (Type m)
decodeStructuralType cx term = do
  typ <- decodeType cx term
  case typeTerm typ of
    TypeTermNominal name -> do
      scx <- schemaContext cx
      el <- requireElement scx name
      decodeStructuralType scx $ elementData el
    _ -> pure typ

freshTypeVariable :: Default m => Infer (Type m) m
freshTypeVariable = do
    s <- get
    put (s + 1)
    return $ Types.variable (h $ normalVariables !! s)
  where
    h (TypeVariable v) = v

generalize :: TypingEnvironment m -> Type m -> TypeScheme m
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

extendEnvironment :: (Variable, TypeScheme m) -> Infer a m -> Infer a m
extendEnvironment (x, sc) m = do
  let scope e = M.insert x sc $ M.delete x e
  local scope m

infer :: (Default m, Ord m, Show m) => Context m -> Data m -> Infer (Data (m, Type m, [Constraint m])) m
infer cx term = case contextTypeOf cx (dataMeta term) of
    ResultSuccess t -> case t of
      Just typ -> do
        i <- inferInternal
        return i { dataMeta = (dataMeta term, typ, [])} -- TODO: unify "suggested" types with inferred types
      Nothing -> inferInternal
  where
    yield expr typ constraints = return Data {
      dataTerm = expr,
      dataMeta = (dataMeta term, typ, constraints)}

    yieldFunction fun = yield (DataTermFunction fun)

    inferInternal = case dataTerm term of
      DataTermApplication (Application fun arg) -> do
        ifun <- infer cx fun
        iarg <- infer cx arg
        v <- freshTypeVariable
        let c = (termConstraints ifun) ++ (termConstraints iarg) ++ [(termType ifun, Types.function (termType iarg) v)]
        let app = DataTermApplication $ Application ifun iarg
        yield app v c

      DataTermElement name -> do
        case typeOfElement cx name of
          -- TODO: polytyped elements will probably be allowed in the future
          ResultSuccess et -> yield (DataTermElement name) (Types.element et) []
          ResultFailure msg -> error msg

      DataTermFunction f -> case f of
        FunctionCases cases -> do
            icases <- CM.mapM (inferFieldType cx) cases
            cod <- freshTypeVariable
            doms <- CM.mapM (\_ -> freshTypeVariable) cases
            let ftypes = termType . fieldData <$> icases
            let ftypes1 = L.zipWith FieldType (fieldName <$> cases) doms
            let innerConstraints = L.concat (termConstraints . fieldData <$> icases)
            let outerConstraints = L.zipWith (\t d -> (t, Types.function d cod)) ftypes doms
            yieldFunction (FunctionCases icases) (Types.function (Types.union ftypes1) cod) (innerConstraints ++ outerConstraints)

        -- Note: here we assume that compareTo evaluates to an integer, not a Comparison value.
        --       For the latter, Comparison would have to be added to the literal type grammar.
        FunctionCompareTo other -> do
          i <- infer cx other
          yieldFunction (FunctionCompareTo i) (Types.function (termType i) Types.int8) (termConstraints i)

        FunctionDelta -> do
          et <- freshTypeVariable
          yieldFunction FunctionDelta (Types.function (Types.element et) et) []

        FunctionLambda (Lambda v body) -> do
          tv <- freshTypeVariable
          i <- extendEnvironment (v, TypeScheme [] tv) (infer cx body)
          yieldFunction (FunctionLambda $ Lambda v i) (Types.function tv (termType i)) (termConstraints i)

        FunctionPrimitive name -> do
          case typeOfPrimitiveFunction cx name of
            ResultSuccess (FunctionType dom cod) -> yieldFunction (FunctionPrimitive name) (Types.function dom cod) []
            ResultFailure msg -> error msg

        -- Note: type inference cannot recover complete record types from projections; type annotations are needed
        FunctionProjection fname -> do
          dom <- freshTypeVariable
          cod <- freshTypeVariable
          let ftype = Types.function (Types.record [FieldType fname dom]) cod
          yieldFunction (FunctionProjection fname) ftype []

        _ -> error $ "type inference is unsupported for function: " ++ show f

      DataTermLet (Let x e1 e2) -> do
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
                yield (DataTermLet $ Let x i1 i2) t2 (c1 ++ c2) -- TODO: is x constant?

      DataTermList els -> do
        v <- freshTypeVariable
        iels <- CM.mapM (infer cx) els
        let co = (\e -> (v, termType e)) <$> iels
        let ci = L.concat (termConstraints <$> iels)
        yield (DataTermList iels) (Types.list v) (co ++ ci)

      DataTermLiteral l -> yield (DataTermLiteral l) (Types.literal $ literalType l) []

      DataTermMap m -> do
          kv <- freshTypeVariable
          vv <- freshTypeVariable
          pairs <- CM.mapM toPair $ M.toList m
          let co = L.concat ((\(k, v) -> [(kv, termType k), (vv, termType v)]) <$> pairs)
          let ci = L.concat ((\(k, v) -> termConstraints k ++ termConstraints v) <$> pairs)
          yield (DataTermMap $ M.fromList pairs) (Types.map kv vv) (co ++ ci)
        where
          toPair (k, v) = do
            ik <- infer cx k
            iv <- infer cx v
            return (ik, iv)

      DataTermNominal (Named name term1) -> do
        case namedType cx name of
          ResultFailure msg -> error msg
          ResultSuccess typ -> do
            i <- infer cx term1
            let typ1 = termType i
            let c = termConstraints i
            yield (DataTermNominal $ Named name i) typ (c ++ [(typ, typ1)])

      DataTermOptional m -> do
        v <- freshTypeVariable
        case m of
          Nothing -> yield (DataTermOptional Nothing) (Types.optional v) []
          Just e -> do
            i <- infer cx e
            yield (DataTermOptional $ Just i) (Types.optional v) ((v, termType i):(termConstraints i))

      DataTermRecord fields -> do
          (fields0, ftypes0, c1) <- CM.foldM forField ([], [], []) fields
          yield (DataTermRecord $ L.reverse fields0) (Types.record $ L.reverse ftypes0) c1
        where
          forField (typed, ftypes, c) field = do
            i <- inferFieldType cx field
            let ft = termType $ fieldData i
            let c1 = termConstraints $ fieldData i
            return (i:typed, (FieldType (fieldName field) ft):ftypes, c1 ++ c)

      DataTermSet els -> do
        v <- freshTypeVariable
        iels <- CM.mapM (infer cx) $ S.toList els
        let co = (\e -> (v, termType e)) <$> iels
        let ci = L.concat (termConstraints <$> iels)
        yield (DataTermSet $ S.fromList iels) (Types.set v) (co ++ ci)

      -- Note: type inference cannot recover complete union types from union values; type annotations are needed
      DataTermUnion field -> do
        ifield <- inferFieldType cx field
        let typ = Types.union [FieldType (fieldName field) (termType $ fieldData ifield)]
        yield (DataTermUnion ifield) typ (termConstraints $ fieldData ifield)

      DataTermVariable x -> do
        t <- lookupTypeInEnvironment x
        yield (DataTermVariable x) t []

      _ -> error $ "type inference is unsupported for term: " ++ show term

inferFieldType :: (Default m, Ord m, Show m) => Context m -> Field m -> Infer (Field (m, Type m, [Constraint m])) m
inferFieldType cx (Field fname term) = Field fname <$> infer cx term

-- | Solve for the toplevel type of an expression in a given environment
inferTop :: (Default m, Ord m, Show m)
  => Context m -> Data m
  -> Either (TypeError m) (Data (m, Type m, [Constraint m]), TypeScheme m)
inferTop cx term = do
    term1 <- runInference (infer cx term)
    let (ResultSuccess scon) = schemaContext cx
    subst <- solveConstraints scon (termConstraints term1)
    let term2 = rewriteDataType (substituteInType subst) term1
    let ts = closeOver $ termType term2
    return (term2, ts)
  where
    -- | Canonicalize and return the polymorphic toplevel type.
    closeOver = normalizeScheme . generalize M.empty

inferType :: (Default m, Ord m, Show m) => Context m -> Data m -> Result (Data (m, Type m, [Constraint m]), TypeScheme m)
inferType cx term = case inferTop cx term of
    Left err -> fail $ "type inference failed: " ++ show err
    Right p -> pure p

instantiate :: Default m => TypeScheme m -> Infer (Type m) m
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshTypeVariable) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

lookupTypeInEnvironment :: Default m => Variable -> Infer (Type m) m
lookupTypeInEnvironment v = do
  env <- ask
  case M.lookup v env of
      Nothing   -> throwError $ UnboundVariable v
      Just s    -> instantiate s

namedType :: (Default m, Show m) => Context m -> Name -> Result (Type m)
namedType cx name = do
  scon <- schemaContext cx
  el <- requireElement scon name
  scon' <- schemaContext scon
  decodeStructuralType scon' $ elementData el

rewriteDataType :: Ord m => (Type m -> Type m) -> Data (m, Type m, [Constraint m]) -> Data (m, Type m, [Constraint m])
rewriteDataType f = rewriteDataMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

runInference :: Infer (Data (m, Type m, [Constraint m])) m -> Either (TypeError m) (Data (m, Type m, [Constraint m]))
runInference term = runExcept $ evalStateT (runReaderT term M.empty) startState

startState :: InferenceState
startState = 0

termConstraints :: Data (m, Type m, [Constraint m]) -> [Constraint m]
termConstraints (Data _ (_, _, constraints)) = constraints

termType :: Data (m, Type m, [Constraint m]) -> Type m
termType (Data _ (_, typ, _)) = typ

typeOfElement :: (Default m, Show m) => Context m -> Name -> Result (Type m)
typeOfElement cx name = do
  el <- requireElement cx name
  decodeStructuralType cx $ elementSchema el

typeOfPrimitiveFunction :: Context m -> Name -> Result (FunctionType m)
typeOfPrimitiveFunction cx name = primitiveFunctionType <$> requirePrimitiveFunction cx name
