-- | Entry point for Hydra's variation on Hindley-Milner type inference

module Hydra.Types.Inference (
  annotateElementWithTypes,
  annotateTermWithTypes,
  inferType,
  inferTypeScheme,
  Constraint,
) where

import Hydra.Kernel
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Types.Substitution
import Hydra.Types.Unification

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type InferenceContext m = (Context m, Int, TypingEnvironment m)

type TypingEnvironment m = M.Map Variable (TypeScheme m)

annotateElementWithTypes :: (Ord m, Show m) => Element m -> GraphFlow m (Element m)
annotateElementWithTypes el = do
    withTrace ("annotate element " ++ unName (elementName el)) $ do
      term <- annotateTermWithTypes $ elementData el
      typ <- findType term
      return $ el {
        elementData = term,
        elementSchema = encodeType typ}
  where
    findType term = do
      cx <- getState
      mt <- annotationClassTermType (contextAnnotations cx) term
      case mt of
        Just t -> return t
        Nothing -> fail "expected a type annotation"

annotateTermWithTypes :: (Ord m, Show m) => Term m -> GraphFlow m (Term m)
annotateTermWithTypes term0 = do
  (term1, _) <- inferType term0
  anns <- contextAnnotations <$> getState
  mt <- annotationClassTermType anns term0 -- Use a provided type, if available, rather than the inferred type
  let annotType (ann, typ, _) = annotationClassSetTypeOf anns (Just $ Y.fromMaybe typ mt) ann
  return $ rewriteTermMeta annotType term1

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: Show m => Term m -> GraphFlow m (Type m)
decodeStructuralType term = do
  typ <- decodeType term
  let typ' = stripType typ
  case typ' of
    TypeNominal name -> withSchemaContext $ withTrace "decode structural type" $ do
      el <- requireElement name
      decodeStructuralType $ elementData el
    _ -> pure typ

freshVariableType :: Flow (InferenceContext m) (Type m)
freshVariableType = do
    (cx, s, e) <- getState
    putState (cx, s + 1, e)
    return $ Types.variable (unVariableType $ normalVariables !! s)

generalize :: Show m => TypingEnvironment m -> Type m -> TypeScheme m
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

extendEnvironment :: Variable -> TypeScheme m -> Flow (InferenceContext m) a -> Flow (InferenceContext m) a
extendEnvironment x sc = withEnvironment (\e -> M.insert x sc $ M.delete x e)

findMatchingField :: Show m => FieldName -> [FieldType m] -> Flow (InferenceContext m) (FieldType m)
findMatchingField fname sfields = case L.filter (\f -> fieldTypeName f == fname) sfields of
  []    -> fail $ "no such field: " ++ unFieldName fname
  (h:_) -> return h

infer :: (Ord m, Show m) => Term m -> Flow (InferenceContext m) (Term (m, Type m, [Constraint m]))
infer term = do
  (cx, _, _) <- getState
  mt <- withGraphContext $ annotationClassTermType (contextAnnotations cx) term
  case mt of
    Just typ -> do
      i <- inferInternal term
      return $ TermAnnotated $ Annotated i (termMeta cx term, typ, []) -- TODO: unify "suggested" types with inferred types
    Nothing -> inferInternal term

inferInternal :: (Ord m, Show m) => Term m -> Flow (InferenceContext m) (Term (m, Type m, [Constraint m]))
inferInternal term = case term of
    TermAnnotated (Annotated term1 ann) -> do
      iterm <- infer term1
      return $ case iterm of
        -- `yield` produces the default annotation, which can just be replaced
        TermAnnotated (Annotated trm (_, t, c)) -> TermAnnotated (Annotated trm (ann, t, c))

    TermApplication (Application fun arg) -> do
      ifun <- infer fun
      iarg <- infer arg
      v <- freshVariableType
      let c = (termConstraints ifun) ++ (termConstraints iarg) ++ [(termType ifun, Types.function (termType iarg) v)]
      let app = TermApplication $ Application ifun iarg
      yield app v c

    TermElement name -> do
      et <- withGraphContext $ typeOfElement name
      yield (TermElement name) (Types.element et) []

    TermFunction f -> case f of

      -- Note: here we assume that compareTo evaluates to an integer, not a Comparison value.
      --       For the latter, Comparison would have to be added to the literal type grammar.
      FunctionCompareTo other -> do
        i <- infer other
        yieldFunction (FunctionCompareTo i) (Types.function (termType i) Types.int8) (termConstraints i)

      FunctionElimination e -> case e of

        EliminationElement -> do
          et <- freshVariableType
          yieldElimination EliminationElement (Types.function (Types.element et) et) []

        EliminationList fun -> do
          a <- freshVariableType
          b <- freshVariableType
          let expected = Types.functionN [b, a] b
          i <- infer fun
          let elim = Types.functionN [b, Types.list a] b
          yieldElimination (EliminationList i) elim [(expected, termType i)]

        EliminationNominal name -> do
          typ <- withGraphContext $ namedType "eliminate nominal" name
          yieldElimination (EliminationNominal name) (Types.function (Types.nominal name) typ) []

        EliminationOptional (OptionalCases n j) -> do
          dom <- freshVariableType
          cod <- freshVariableType
          ni <- infer n
          ji <- infer j
          let t = Types.function (Types.optional dom) cod
          let constraints = [(cod, termType ni), (Types.function dom cod, termType ji)]
          yieldElimination (EliminationOptional $ OptionalCases ni ji) t constraints

        -- Note: type inference cannot recover complete record types from projections; type annotations are needed
        EliminationRecord (Projection name fname) -> do
          rt <- withGraphContext $ requireRecordType True name
          sfield <- findMatchingField fname (rowTypeFields rt)
          yieldElimination (EliminationRecord $ Projection name fname)
            (Types.function (TypeRecord rt) $ fieldTypeType sfield) []

        EliminationUnion (CaseStatement name cases) -> do
            rt <- withGraphContext $ requireUnionType True name
            let sfields = rowTypeFields rt

            icases <- CM.mapM inferFieldType cases
            let innerConstraints = L.concat (termConstraints . fieldTerm <$> icases)

            let idoms = termType . fieldTerm <$> icases
            let sdoms = fieldTypeType <$> sfields
            cod <- freshVariableType
            let outerConstraints = L.zipWith (\t d -> (t, Types.function d cod)) idoms sdoms

            yieldElimination (EliminationUnion (CaseStatement name icases))
              (Types.function (TypeUnion rt) cod)
              (innerConstraints ++ outerConstraints)

      FunctionLambda (Lambda v body) -> do
        tv <- freshVariableType
        i <- extendEnvironment v (TypeScheme [] tv) $ infer body
        yieldFunction (FunctionLambda $ Lambda v i) (Types.function tv (termType i)) (termConstraints i)

      FunctionPrimitive name -> do
        FunctionType dom cod <- withGraphContext $ typeOfPrimitiveFunction name
        yieldFunction (FunctionPrimitive name) (Types.function dom cod) []

    TermLet (Let x e1 e2) -> do
      (_, _, env) <- getState
      i1 <- infer e1
      let t1 = termType i1
      let c1 = termConstraints i1
      sub <- withGraphContext $ solveConstraints c1
      let t1' = reduceType $ substituteInType sub t1
      let sc = generalize (M.map (substituteInScheme sub) env) t1'
      i2 <- extendEnvironment x sc $ withEnvironment (M.map (substituteInScheme sub)) $ infer e2
      let t2 = termType i2
      let c2 = termConstraints i2
      yield (TermLet $ Let x i1 i2) t2 (c1 ++ c2) -- TODO: is x constant?

    TermList els -> do
      v <- freshVariableType
      iels <- CM.mapM infer els
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
          ik <- infer k
          iv <- infer v
          return (ik, iv)

    TermNominal (Named name term1) -> do
      typ <- withGraphContext $ namedType "nominal" name
      i <- infer term1
      yield (TermNominal $ Named name i) (Types.nominal name) (termConstraints i ++ [(typ, termType i)])

    TermOptional m -> do
      v <- freshVariableType
      case m of
        Nothing -> yield (TermOptional Nothing) (Types.optional v) []
        Just e -> do
          i <- infer e
          yield (TermOptional $ Just i) (Types.optional v) ((v, termType i):(termConstraints i))

    TermProduct tuple -> do
      is <- CM.mapM infer tuple
      yield (TermProduct is) (TypeProduct $ fmap termType is) (L.concat $ fmap termConstraints is)

    TermRecord (Record n fields) -> do
        rt <- withGraphContext $ requireRecordType True n
        let sfields = rowTypeFields rt
        (fields0, ftypes0, c1) <- CM.foldM forField ([], [], []) $ L.zip fields sfields
        yield (TermRecord $ Record n $ L.reverse fields0) (TypeRecord $ RowType n (rowTypeExtends rt) $ L.reverse ftypes0) c1
      where
        forField (typed, ftypes, c) (field, sfield) = do
          i <- inferFieldType field
          let ft = termType $ fieldTerm i
          let cinternal = termConstraints $ fieldTerm i
          let cnominal = (ft, fieldTypeType sfield)
          return (i:typed, (FieldType (fieldName field) ft):ftypes, cnominal:(cinternal ++ c))

    TermSet els -> do
      v <- freshVariableType
      iels <- CM.mapM infer $ S.toList els
      let co = (\e -> (v, termType e)) <$> iels
      let ci = L.concat (termConstraints <$> iels)
      yield (TermSet $ S.fromList iels) (Types.set v) (co ++ ci)

    TermSum (Sum i s trm) -> do
        it <- infer trm
        types <- CM.sequence (varOrTerm it <$> [0..(s-1)])
        yield (TermSum $ Sum i s it) (TypeSum types) (termConstraints it)
      where
        varOrTerm it j = if i == j
          then pure $ termType it
          else freshVariableType

    -- Note: type inference cannot recover complete union types from union values; type annotations are needed
    TermUnion (Union n field) -> do
        rt <- withGraphContext $ requireUnionType True n
        sfield <- findMatchingField (fieldName field) (rowTypeFields rt)
        ifield <- inferFieldType field
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

    yield term typ constraints = do
      (cx, _, _) <- getState
      return $ TermAnnotated $ Annotated term (annotationClassDefault $ contextAnnotations cx, typ, constraints)

inferFieldType :: (Ord m, Show m) => Field m -> Flow (InferenceContext m) (Field (m, Type m, [Constraint m]))
inferFieldType (Field fname term) = Field fname <$> infer term

-- | Solve for the top-level type of an expression in a given environment
inferType :: (Ord m, Show m) => Term m -> GraphFlow m (Term (m, Type m, [Constraint m]), TypeScheme m)
inferType term = do
    withTrace ("infer type") $ do
      cx <- getState
      withState (startContext cx) $ do
        term1 <- infer term
        subst <- withGraphContext $ withSchemaContext $ solveConstraints (termConstraints term1)
        let term2 = rewriteDataType (substituteInType subst) term1
        return (term2, closeOver $ termType term2)
  where
    -- | Canonicalize and return the polymorphic top-level type.
    closeOver = normalizeScheme . generalize M.empty . reduceType

inferTypeScheme :: (Ord m, Show m) => Term m -> GraphFlow m (TypeScheme m)
inferTypeScheme term = snd <$> inferType term

instantiate :: TypeScheme m -> Flow (InferenceContext m) (Type m)
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshVariableType) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

lookupTypeInEnvironment :: Show m => Variable -> Flow (InferenceContext m) (Type m)
lookupTypeInEnvironment v = do
  (_, _, env) <- getState
  case M.lookup v env of
    Nothing -> fail $ "unbound variable: " ++ unVariable v
    Just s  -> instantiate s

namedType :: Show m => String -> Name -> GraphFlow m (Type m)
namedType debug name = do
  withTrace (debug ++ ": " ++ unName name) $ do
    withSchemaContext $ do
      el <- requireElement name
      decodeStructuralType $ elementData el

reduceType :: (Ord m, Show m) => Type m -> Type m
reduceType t = t -- betaReduceType cx t

rewriteDataType :: Ord m => (Type m -> Type m) -> Term (m, Type m, [Constraint m]) -> Term (m, Type m, [Constraint m])
rewriteDataType f = rewriteTermMeta rewrite
  where
    rewrite (x, typ, c) = (x, f typ, c)

startContext :: Context m -> InferenceContext m
startContext cx = (cx, 0, M.empty)

termConstraints :: Term (m, Type m, [Constraint m]) -> [Constraint m]
termConstraints (TermAnnotated (Annotated _ (_, _, constraints))) = constraints

termType :: Term (m, Type m, [Constraint m]) -> Type m
termType (TermAnnotated (Annotated _ (_, typ, _))) = typ

typeOfElement :: Show m => Name -> GraphFlow m (Type m)
typeOfElement name = withTrace "type of element" $ do
  el <- requireElement name
  decodeStructuralType $ elementSchema el

typeOfPrimitiveFunction :: Name -> GraphFlow m (FunctionType m)
typeOfPrimitiveFunction name = primitiveFunctionType <$> requirePrimitiveFunction name

withEnvironment :: (TypingEnvironment m -> TypingEnvironment m) -> Flow (InferenceContext m) a -> Flow (InferenceContext m) a
withEnvironment m f = do
  (cx, i, e) <- getState
  withState (cx, i, m e) f

withGraphContext :: GraphFlow m a -> Flow (InferenceContext m) a
withGraphContext f = do
  (cx, _, _) <- getState
  withState cx f
