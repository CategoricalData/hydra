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
import Hydra.Reduction

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
decodeStructuralType :: (Show m) => Context m -> Term m -> Result (Type m)
decodeStructuralType cx term = do
  typ <- decodeType cx term
  case typeExpr cx typ of
    TypeNominal name -> do
      let scx = schemaContext cx
      el <- requireElement (Just "decode structural type") scx name
      decodeStructuralType scx $ elementData el
    _ -> pure typ

freshVariableType :: Infer (Type m) m
freshVariableType = do
    s <- get
    put (s + 1)
    return $ Types.variable (h $ normalVariables !! s)
  where
    h (VariableType v) = v

generalize :: Show m => TypingEnvironment m -> Type m -> TypeScheme m
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

extendEnvironment :: (Variable, TypeScheme m) -> Infer a m -> Infer a m
extendEnvironment (x, sc) m = do
  let scope e = M.insert x sc $ M.delete x e
  local scope m

findMatchingField :: Context m -> FieldName -> [FieldType m] -> Infer (FieldType m) m
findMatchingField cx fname sfields = case L.filter (\f -> fieldTypeName f == fname) sfields of
  []    -> throwError $ OtherError (printTrace cx) $ "no such field: " ++ unFieldName fname
  (h:_) -> return h

infer :: (Ord m, Show m) => Context m -> Term m -> Infer (Term (m, Type m, [Constraint m])) m
infer cx term = case annotationClassTermType (contextAnnotations cx) cx term of
    ResultSuccess t -> case t of
      Just typ -> do
        i <- inferInternal cx term
        return $ TermAnnotated $ Annotated i (termMeta cx term, typ, []) -- TODO: unify "suggested" types with inferred types
      Nothing -> inferInternal cx term

inferInternal :: (Ord m, Show m) => Context m -> Term m -> Infer (Term (m, Type m, [Constraint m])) m
inferInternal cx term = case termExpr cx term of
    TermApplication (Application fun arg) -> do
      ifun <- infer cx fun
      iarg <- infer cx arg
      v <- freshVariableType
      let c = (termConstraints ifun) ++ (termConstraints iarg) ++ [(termType ifun, Types.function (termType iarg) v)]
      let app = TermApplication $ Application ifun iarg
      yield app v c

    TermElement name -> do
      case typeOfElement cx name of
        -- TODO: polytyped elements will probably be allowed in the future
        ResultSuccess et -> yield (TermElement name) (Types.element et) []
        ResultFailure msg -> error msg

    TermFunction f -> case f of

      -- Note: here we assume that compareTo evaluates to an integer, not a Comparison value.
      --       For the latter, Comparison would have to be added to the literal type grammar.
      FunctionCompareTo other -> do
        i <- infer cx other
        yieldFunction (FunctionCompareTo i) (Types.function (termType i) Types.int8) (termConstraints i)

      FunctionElimination e -> case e of

        EliminationElement -> do
          et <- freshVariableType
          yieldElimination EliminationElement (Types.function (Types.element et) et) []

        EliminationNominal name -> do
          case namedType "eliminate nominal" cx name of
            ResultFailure msg -> error msg
            ResultSuccess typ -> yieldElimination (EliminationNominal name) (Types.function (Types.nominal name) typ) []

        EliminationOptional (OptionalCases n j) -> do
          dom <- freshVariableType
          cod <- freshVariableType
          ni <- infer cx n
          ji <- infer cx j
          let t = Types.function (Types.optional dom) cod
          let constraints = [(cod, termType ni), (Types.function dom cod, termType ji)]
          yieldElimination (EliminationOptional $ OptionalCases ni ji) t constraints

        -- Note: type inference cannot recover complete record types from projections; type annotations are needed
        EliminationRecord (Projection name fname) -> do
          rt <- resultToInfer cx $ requireRecordType cx name
          sfield <- findMatchingField cx fname (rowTypeFields rt)
          yieldElimination (EliminationRecord $ Projection name fname)
            (Types.function (TypeRecord rt) $ fieldTypeType sfield) []

        EliminationUnion (CaseStatement name cases) -> do
            rt <- resultToInfer cx $ requireUnionType cx name
            let sfields = rowTypeFields rt

            icases <- CM.mapM (inferFieldType cx) cases
            let innerConstraints = L.concat (termConstraints . fieldTerm <$> icases)

            let idoms = termType . fieldTerm <$> icases
            let sdoms = fieldTypeType <$> sfields
            cod <- freshVariableType
            let outerConstraints = L.zipWith (\t d -> (t, Types.function d cod)) idoms sdoms

            yieldElimination (EliminationUnion (CaseStatement name  icases))
              (Types.function (TypeUnion rt) cod)
              (innerConstraints ++ outerConstraints)

      FunctionLambda (Lambda v body) -> do
        tv <- freshVariableType
        i <- extendEnvironment (v, TypeScheme [] tv) (infer cx body)
        yieldFunction (FunctionLambda $ Lambda v i) (Types.function tv (termType i)) (termConstraints i)

      FunctionPrimitive name -> do
        case typeOfPrimitiveFunction cx name of
          ResultSuccess (FunctionType dom cod) -> yieldFunction (FunctionPrimitive name) (Types.function dom cod) []
          ResultFailure msg -> error msg

    TermLet (Let x e1 e2) -> do
      env <- ask
      i1 <- infer cx e1
      let t1 = termType i1
      let c1 = termConstraints i1
      case solveConstraints cx c1 of
          Left err -> throwError err
          Right sub -> do
              let scx = schemaContext cx
              let t1' = reduceType scx $ substituteInType sub t1
              let sc = generalize (M.map (substituteInScheme sub) env) t1'
              i2 <- extendEnvironment (x, sc) $ local (M.map (substituteInScheme sub)) (infer cx e2)
              let t2 = termType i2
              let c2 = termConstraints i2
              yield (TermLet $ Let x i1 i2) t2 (c1 ++ c2) -- TODO: is x constant?

    TermList els -> do
      v <- freshVariableType
      iels <- CM.mapM (infer cx) els
      let co = (\e -> (v, termType e)) <$> iels
      let ci = L.concat (termConstraints <$> iels)
      yield (TermList iels) (Types.list v) (co ++ ci)

    TermLiteral l -> yield (TermLiteral l) (Types.literal $ literalType l) []

    TermMap m -> do
        kv <- freshVariableType
        vv <- freshVariableType
        pairs <- CM.mapM toPair $ M.toList m
        let co = L.concat ((\(k, v) -> [(kv, termType k), (vv, termType v)]) <$> pairs)
        let ci = L.concat ((\(k, v) -> termConstraints k ++ termConstraints v) <$> pairs)
        yield (TermMap $ M.fromList pairs) (Types.map kv vv) (co ++ ci)
      where
        toPair (k, v) = do
          ik <- infer cx k
          iv <- infer cx v
          return (ik, iv)

    TermNominal (Named name term1) -> do
      case namedType "nominal" cx name of
        ResultFailure msg -> error msg
        ResultSuccess typ -> do
          i <- infer cx term1
          let typ1 = termType i
          let c = termConstraints i
          yield (TermNominal $ Named name i) (Types.nominal name) (c ++ [(typ, typ1)])

    TermOptional m -> do
      v <- freshVariableType
      case m of
        Nothing -> yield (TermOptional Nothing) (Types.optional v) []
        Just e -> do
          i <- infer cx e
          yield (TermOptional $ Just i) (Types.optional v) ((v, termType i):(termConstraints i))

    TermRecord (Record n fields) -> do
        rt <- resultToInfer cx $ requireRecordType cx n
        let sfields = rowTypeFields rt
        (fields0, ftypes0, c1) <- CM.foldM forField ([], [], []) $ L.zip fields sfields
        yield (TermRecord $ Record n $ L.reverse fields0) (TypeRecord $ RowType n $ L.reverse ftypes0) c1
      where
        forField (typed, ftypes, c) (field, sfield) = do
          i <- inferFieldType cx field
          let ft = termType $ fieldTerm i
          let cinternal = termConstraints $ fieldTerm i
          let cnominal = (ft, fieldTypeType sfield)
          return (i:typed, (FieldType (fieldName field) ft):ftypes, cnominal:(cinternal ++ c))

    TermSet els -> do
      v <- freshVariableType
      iels <- CM.mapM (infer cx) $ S.toList els
      let co = (\e -> (v, termType e)) <$> iels
      let ci = L.concat (termConstraints <$> iels)
      yield (TermSet $ S.fromList iels) (Types.set v) (co ++ ci)

    -- Note: type inference cannot recover complete union types from union values; type annotations are needed
    TermUnion (Union n field) -> do
        rt <- resultToInfer cx $ requireUnionType cx n
        sfield <- findMatchingField cx (fieldName field) (rowTypeFields rt)
        ifield <- inferFieldType cx field
        let cinternal = termConstraints $ fieldTerm ifield
        let cnominal = (termType $ fieldTerm ifield, fieldTypeType sfield)
        let constraints = cnominal:cinternal
        yield (TermUnion $ Union n ifield) (TypeUnion rt) constraints

    TermVariable x -> do
      t <- lookupTypeInEnvironment x
      yield (TermVariable x) t []
  where
    yieldFunction fun = yield (TermFunction fun)

    yieldElimination e = yield (TermFunction $ FunctionElimination e)

    yield term typ constraints = case term of
      TermAnnotated (Annotated term' (meta, _, _)) -> return $ TermAnnotated $ Annotated term' (meta, typ, constraints)
      _ -> return $ TermAnnotated $ Annotated term (annotationClassDefault $ contextAnnotations cx, typ, constraints)

inferFieldType :: (Ord m, Show m) => Context m -> Field m -> Infer (Field (m, Type m, [Constraint m])) m
inferFieldType cx (Field fname term) = Field fname <$> infer cx term

-- | Solve for the toplevel type of an expression in a given environment
inferTop :: (Ord m, Show m)
  => Context m -> Term m
  -> Either (TypeError m) (Term (m, Type m, [Constraint m]), TypeScheme m)
inferTop cx term = do
    term1 <- runInference (infer cx term)
    let scx = schemaContext cx
    subst <- solveConstraints scx (termConstraints term1)
    let term2 = rewriteDataType (substituteInType subst) term1
    let ts = closeOver scx $ termType term2
    return (term2, ts)
  where
    -- | Canonicalize and return the polymorphic toplevel type.
    closeOver scx = normalizeScheme . generalize M.empty . reduceType scx

inferType :: (Ord m, Show m) => Context m -> Term m -> Result (Term (m, Type m, [Constraint m]), TypeScheme m)
inferType cx0 term = case inferTop cx term of
    Left err -> fail $ "type inference failed: " ++ show err
    Right p -> pure p
  where
--    cx = pushTrace "infer type" cx0
    cx = pushTrace ("infer type of " ++ show term) cx0

instantiate :: TypeScheme m -> Infer (Type m) m
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshVariableType) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

lookupTypeInEnvironment :: Variable -> Infer (Type m) m
lookupTypeInEnvironment v = do
  env <- ask
  case M.lookup v env of
      Nothing   -> throwError $ UnboundVariable v
      Just s    -> instantiate s

namedType :: (Show m) => String -> Context m -> Name -> Result (Type m)
namedType debug cx name = do
  el <- requireElement (Just debug) cx name
  let scx = schemaContext cx
  decodeStructuralType scx $ elementData el

reduceType :: (Ord m, Show m) => Context m -> Type m -> Type m
reduceType cx t = t -- betaReduceType cx t

resultToInfer :: Context m -> Result a -> Infer a m
resultToInfer cx r = case r of
  ResultSuccess x -> pure x
  ResultFailure e -> throwError $ OtherError (printTrace cx) e

rewriteDataType :: Ord m => (Type m -> Type m) -> Term (m, Type m, [Constraint m]) -> Term (m, Type m, [Constraint m])
rewriteDataType f = rewriteTermMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

runInference :: Infer (Term (m, Type m, [Constraint m])) m -> Either (TypeError m) (Term (m, Type m, [Constraint m]))
runInference term = runExcept $ evalStateT (runReaderT term M.empty) startState

startState :: InferenceState
startState = 0

termConstraints :: Term (m, Type m, [Constraint m]) -> [Constraint m]
termConstraints (TermAnnotated (Annotated _ (_, _, constraints))) = constraints

termType :: Term (m, Type m, [Constraint m]) -> Type m
termType (TermAnnotated (Annotated _ (_, typ, _))) = typ

typeOfElement :: (Show m) => Context m -> Name -> Result (Type m)
typeOfElement cx name = do
  el <- requireElement (Just "type of element") cx name
  decodeStructuralType cx $ elementSchema el

typeOfPrimitiveFunction :: Context m -> Name -> Result (FunctionType m)
typeOfPrimitiveFunction cx name = primitiveFunctionType <$> requirePrimitiveFunction cx name
