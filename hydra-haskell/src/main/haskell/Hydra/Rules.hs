-- | Inference rules

module Hydra.Rules where

import Hydra.Basics
import Hydra.Strip
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Rewriting
import Hydra.Substitution
import Hydra.Unification
import Hydra.Tools.Debug
import Hydra.Annotations
import Hydra.Tier1
import Hydra.Tier2
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data InferenceContext = InferenceContext {
  inferenceContextGraph :: Graph,
  inferenceContextEnvironment :: TypingEnvironment}

data Inferred a = Inferred {
  -- The original term, possibly annotated with the inferred type
  inferredObject :: a,
  -- The inferred type
  inferredType :: Type,
  -- Any constraints introduced by the inference process
  inferredConstraints :: [Constraint]
} deriving Show

type TypingEnvironment = M.Map Name TypeScheme

fieldType :: Field -> FieldType
fieldType (Field fname term) = FieldType fname $ termType term

findMatchingField :: Name -> [FieldType] -> Flow InferenceContext FieldType
findMatchingField fname sfields = case L.filter (\f -> fieldTypeName f == fname) sfields of
  []    -> fail $ "no such field: " ++ unName fname
  (h:_) -> return h

freshName :: Flow InferenceContext Type
freshName = TypeVariable . normalVariable <$> nextCount "hyInf"

generalize :: TypingEnvironment -> Type -> TypeScheme
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

infer :: Term -> Flow InferenceContext (Inferred Term)
infer term = withTrace ("infer for " ++ show (termVariant term)) $ case term of
    TermAnnotated (AnnotatedTerm term1 ann) -> do
      (Inferred term2 typ constraints) <- infer term1
      return (Inferred (TermAnnotated $ AnnotatedTerm term2 ann) typ constraints)

    TermTyped (TypedTerm term1 typ) -> do
      (Inferred i typ c) <- infer term1
      return (Inferred (setTermType (Just typ) i) typ $ c ++ [(typ, termType i)])

    TermApplication (Application fun arg) -> do
      (Inferred ifun ityp funconst) <- infer fun
      (Inferred iarg atyp argconst) <- infer arg
      cod <- freshName
      let constraints = funconst ++ argconst ++ [(ityp, Types.function atyp cod)]
      yield (TermApplication $ Application ifun iarg) cod constraints

    TermFunction f -> case f of

      FunctionElimination e -> case e of

        EliminationList fun -> do
          a <- freshName
          b <- freshName
          let expected = Types.functionN [b, a, b]
          (Inferred i t c) <- infer fun
          let elim = Types.functionN [b, Types.list a, b]
          yieldElimination (EliminationList i) elim (c ++ [(expected, t)])

        EliminationOptional (OptionalCases n j) -> do
          dom <- freshName
          cod <- freshName
          (Inferred ni nt nconst) <- infer n
          (Inferred ji jt jconst) <- infer j
          let t = Types.function (Types.optional dom) cod
          let constraints = nconst ++ jconst
                              ++ [(cod, nt), (Types.function dom cod, jt)]
          yieldElimination (EliminationOptional $ OptionalCases ni ji) t constraints

        EliminationProduct (TupleProjection arity idx) -> do
          types <- CM.replicateM arity freshName
          let cod = types !! idx
          let t = Types.function (Types.product types) cod
          yieldElimination (EliminationProduct $ TupleProjection arity idx) t []

        EliminationRecord (Projection name fname) -> do
          rt <- withGraphContext $ requireRecordType True name
          sfield <- findMatchingField fname (rowTypeFields rt)
          yieldElimination (EliminationRecord $ Projection name fname)
            (Types.function (TypeRecord rt) $ fieldTypeType sfield) []

        EliminationUnion (CaseStatement tname def cases) -> do
            -- Default value
            (idef, dfltConstraints) <- case def of
              Nothing -> pure (Nothing, [])
              Just d -> do
                (Inferred i _ c) <- infer d
                return (Just i, c)

            -- Cases
            icases' <- CM.mapM inferFieldType cases
            let icases = inferredObject <$> icases'
            let casesconst = inferredConstraints <$> icases'
            let icasesMap = fieldMap icases
            rt <- withGraphContext $ requireUnionType True tname
            let sfields = fieldTypeMap  $ rowTypeFields rt
            checkCasesAgainstSchema tname icasesMap sfields
            let pairMap = productOfMaps icasesMap sfields

            cod <- freshName
            let outerConstraints = (\(d, s) -> (termType d, Types.function s cod)) <$> M.elems pairMap
            let innerConstraints = dfltConstraints ++ L.concat casesconst

            yieldElimination (EliminationUnion (CaseStatement tname idef icases))
              (Types.function (TypeUnion rt) cod)
              (innerConstraints ++ outerConstraints)
          where
            checkCasesAgainstSchema tname icases sfields = if M.null diff
                then pure ()
                else fail $ "case(s) in case statement which do not exist in type " ++ unName tname ++ ": "
                  ++ L.intercalate ", " (unName <$> M.keys diff)
              where
                diff = M.difference icases sfields

        EliminationWrap name -> do
          typ <- withGraphContext $ requireWrappedType name
          yieldElimination (EliminationWrap name) (Types.function (TypeWrap $ WrappedType name typ) typ) []

      FunctionLambda (Lambda v body) -> do
        tv <- freshName
        (Inferred i t iconst) <- withBinding v (monotype tv) $ infer body
        yieldFunction (FunctionLambda $ Lambda v i) (Types.function tv t) iconst

      FunctionPrimitive name -> do
          t <- (withGraphContext $ typeOfPrimitive name) >>= replaceFreeVariables
          yieldFunction (FunctionPrimitive name) t []
        where
          -- This prevents type variables from being reused across multiple instantiations of a primitive within a single element,
          -- which would lead to false unification.
          replaceFreeVariables t = do
              pairs <- CM.mapM toPair $ S.toList $ freeVariablesInType t
              return $ substituteInType (M.fromList pairs) t
            where
              toPair v = do
                v' <- freshName
                return (v, v')

    TermLet lt -> inferLet lt

    TermList els -> do
        v <- freshName
        if L.null els
          then yield (TermList []) (Types.list v) []
          else do
            iels' <- CM.mapM infer els
            let iels = inferredObject <$> iels'
            let elsconst = inferredConstraints <$> iels'
            let co = (\e -> (v, termType e)) <$> iels
            let ci = L.concat elsconst
            yield (TermList iels) (Types.list v) (co ++ ci)

    TermLiteral l -> yield (TermLiteral l) (Types.literal $ literalType l) []

    TermMap m -> do
        kv <- freshName
        vv <- freshName
        if M.null m
          then yield (TermMap M.empty) (Types.map kv vv) []
          else do
            triples <- CM.mapM toTriple $ M.toList m
            let pairs = (\(k, v, _) -> (k, v)) <$> triples
            let co = L.concat ((\(k, v, c) -> c ++ [(kv, termType k), (vv, termType v)]) <$> triples)
            yield (TermMap $ M.fromList pairs) (Types.map kv vv) co
      where
        toTriple (k, v) = do
          (Inferred ik _ kc) <- infer k
          (Inferred iv _ vc) <- infer v
          return (ik, iv, kc ++ vc)

    TermOptional m -> do
      v <- freshName
      case m of
        Nothing -> yield (TermOptional Nothing) (Types.optional v) []
        Just e -> do
          (Inferred i t ci) <- infer e
          yield (TermOptional $ Just i) (Types.optional v) ((v, t):ci)

    TermProduct tuple -> do
      is' <- CM.mapM infer tuple
      let is = inferredObject <$> is'
      let co = L.concat (inferredConstraints <$> is')
      yield (TermProduct is) (TypeProduct $ fmap termType is) co

    TermRecord (Record n fields) -> do
        rt <- withGraphContext $ requireRecordType True n
        ifields' <- CM.mapM inferFieldType fields
        let ifields = inferredObject <$> ifields'
        let ci = L.concat (inferredConstraints <$> ifields')
        let irt = TypeRecord $ RowType n Nothing (fieldType <$> ifields)
        yield (TermRecord $ Record n ifields) irt ((TypeRecord rt, irt):ci)

    TermSet els -> do
      v <- freshName
      if S.null els
        then yield (TermSet S.empty) (Types.set v) []
        else do
          iels' <- CM.mapM infer $ S.toList els
          let iels = inferredObject <$> iels'
          let co = (\e -> (v, termType e)) <$> iels
          let ci = L.concat (inferredConstraints <$> iels')
          yield (TermSet $ S.fromList iels) (Types.set v) (co ++ ci)

    TermSum (Sum i s trm) -> do
        (Inferred it t co) <- infer trm
        types <- CM.sequence (varOrTerm t <$> [0..(s-1)])
        yield (TermSum $ Sum i s it) (TypeSum types) co
      where
        varOrTerm t j = if i == j
          then pure t
          else freshName

    TermUnion (Injection n field) -> do
        rt <- withGraphContext $ requireUnionType True n
        sfield <- findMatchingField (fieldName field) (rowTypeFields rt)
        (Inferred ifield t ci) <- inferFieldType field
        let co = (t, fieldTypeType sfield)

        yield (TermUnion $ Injection n ifield) (TypeUnion rt) (co:ci)

    TermVariable v -> do
      t <- requireName v
      yield (TermVariable v) t []

    TermWrap (WrappedTerm name term1) -> do
      typ <- withGraphContext $ requireWrappedType name
      (Inferred i t ci) <- infer term1
      yield (TermWrap $ WrappedTerm name i) (TypeWrap $ WrappedType name typ) (ci ++ [(typ, t)])

inferFieldType :: Field -> Flow InferenceContext (Inferred Field)
inferFieldType (Field fname term) = do
  (Inferred i t c) <- infer term
  return (Inferred (Field fname i) t c)

inferLet :: Let -> Flow InferenceContext (Inferred Term)
inferLet (Let bindings env) = withTrace ("let(" ++ L.intercalate "," (unName . letBindingName <$> bindings) ++ ")") $ do
    state0 <- getState
    let e = preExtendEnv bindings $ inferenceContextEnvironment state0
    let state1 = state0 {inferenceContextEnvironment = e}
    withState state1 $ do
      -- TODO: perform a topological sort on the bindings; this process should be unified with that of elements in a graph

      -- Infer types of bindings in the pre-extended environment
      ivalues' <- CM.mapM infer (letBindingTerm <$> bindings)
      let ivalues = inferredObject <$> ivalues'
      let ibindings = L.zipWith (\(LetBinding k v t) i -> LetBinding k i t) bindings ivalues
      let bc = L.concat (inferredConstraints <$> ivalues')

      let tbindings = M.fromList $ fmap (\(LetBinding k i t) -> (k, termTypeScheme i)) ibindings
      (Inferred ienv t cenv) <- withBindings tbindings $ infer env

      yield (TermLet $ Let ibindings ienv) t (bc ++ cenv)
  where
    -- Add any manual type annotations for the bindings to the environment, enabling type inference over recursive definitions
    preExtendEnv bindings e = foldl addPair e bindings
      where
        addPair e (LetBinding name term _) = case typeOfTerm term of
          Nothing -> e
          Just typ -> M.insert name (monotype typ) e

instantiate :: TypeScheme -> Flow InferenceContext Type
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshName) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

monotype :: Type -> TypeScheme
monotype typ = TypeScheme [] typ

productOfMaps :: Ord k => M.Map k l -> M.Map k r -> M.Map k (l, r)
productOfMaps ml mr = M.fromList $ Y.catMaybes (toPair <$> M.toList mr)
  where
    toPair (k, vr) = (\vl -> (k, (vl, vr))) <$> M.lookup k ml

reduceType :: Type -> Type
reduceType t = t -- betaReduceType cx t

requireName :: Name -> Flow InferenceContext Type
requireName v = do
  env <- inferenceContextEnvironment <$> getState
  case M.lookup v env of
    Nothing -> fail $ "variable not bound in environment: " ++ unName v ++ ". Environment: "
      ++ L.intercalate ", " (unName <$> M.keys env)
    Just s  -> instantiate s

termType :: Term -> Type
termType term = case stripTerm term of
  (TermTyped (TypedTerm _ typ)) -> typ

-- TODO: limited and temporary
termTypeScheme :: Term -> TypeScheme
termTypeScheme = monotype . termType

typeOfPrimitive :: Name -> Flow Graph Type
typeOfPrimitive name = primitiveType <$> requirePrimitive name

typeOfTerm :: Term -> Maybe Type
typeOfTerm term = case term of
  TermAnnotated (AnnotatedTerm term1 _) -> typeOfTerm term1
  TermTyped (TypedTerm term1 typ) -> Just typ
  _ -> Nothing

withBinding :: Name -> TypeScheme -> Flow InferenceContext x -> Flow InferenceContext x
withBinding n ts = withEnvironment (M.insert n ts)

withBindings :: M.Map Name TypeScheme -> Flow InferenceContext x -> Flow InferenceContext x
withBindings bindings = withEnvironment (\e -> M.union bindings e)

withEnvironment :: (TypingEnvironment -> TypingEnvironment) -> Flow InferenceContext x -> Flow InferenceContext x
withEnvironment m flow = do
  InferenceContext g e <- getState
  withState (InferenceContext g (m e)) flow

withGraphContext :: Flow Graph x -> Flow InferenceContext x
withGraphContext f = do
  cx <- inferenceContextGraph <$> getState
  withState cx f

yield :: Term -> Type -> [Constraint] -> Flow InferenceContext (Inferred Term)
yield term typ constraints = do
  return (Inferred (TermTyped $ TypedTerm term typ) typ constraints)

yieldFunction :: Function -> Type -> [Constraint] -> Flow InferenceContext (Inferred Term)
yieldFunction fun = yield (TermFunction fun)

yieldElimination :: Elimination -> Type -> [Constraint] -> Flow InferenceContext (Inferred Term)
yieldElimination e = yield (TermFunction $ FunctionElimination e)
