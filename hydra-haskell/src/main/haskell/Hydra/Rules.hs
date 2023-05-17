-- | Inference rules

module Hydra.Rules where

import Hydra.Basics
import Hydra.Common
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.CoreEncoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Flows
import Hydra.Rewriting
import Hydra.Substitution
import Hydra.Unification
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


type InfAnn a = (a, Type a, [Constraint a])

data InferenceContext a = InferenceContext {
  inferenceContextGraph :: Graph a,
  inferenceContextCounter :: Int,
  inferenceContextEnvironment :: TypingEnvironment a}

type TypingEnvironment a = M.Map Name (TypeScheme a)

applyRules :: (Ord a, Show a) => Term a -> Flow (InferenceContext a) (Term (InfAnn a))
applyRules term = case term of
    TermAnnotated (Annotated term1 ann) -> do
      iterm <- infer term1
      return $ case iterm of
        -- `yield` produces the default annotation, which can just be replaced
        TermAnnotated (Annotated trm (_, t, c)) -> TermAnnotated (Annotated trm (ann, t, c))

    TermApplication (Application fun arg) -> do
      ifun <- infer fun
      iarg <- infer arg
      cod <- freshName
      let constraints = (termConstraints ifun) ++ (termConstraints iarg) ++ [(termType ifun, Types.function (termType iarg) cod)]
      yield (TermApplication $ Application ifun iarg) cod constraints

    TermElement name -> do
      et <- withGraphContext $ typeOfElement name
      yield (TermElement name) (TypeElement et) []

    TermFunction f -> case f of

      FunctionElimination e -> case e of

        EliminationElement -> do
          et <- freshName
          yieldElimination EliminationElement (Types.function (TypeElement et) et) []

        EliminationList fun -> do
          a <- freshName
          b <- freshName
          let expected = Types.functionN [b, a] b
          i <- infer fun
          let elim = Types.functionN [b, Types.list a] b
          yieldElimination (EliminationList i) elim [(expected, termType i)]

        EliminationOptional (OptionalCases n j) -> do
          dom <- freshName
          cod <- freshName
          ni <- infer n
          ji <- infer j
          let t = Types.function (Types.optional dom) cod
          let constraints = [(cod, termType ni), (Types.function dom cod, termType ji)]
          yieldElimination (EliminationOptional $ OptionalCases ni ji) t constraints

        EliminationRecord (Projection name fname) -> do
          rt <- withGraphContext $ requireRecordType True name
          sfield <- findMatchingField fname (rowTypeFields rt)
          yieldElimination (EliminationRecord $ Projection name fname)
            (Types.function (TypeRecord rt) $ fieldTypeType sfield) []

        EliminationUnion (CaseStatement name def cases) -> do
            rt <- withGraphContext $ requireUnionType True name
            let sfields = rowTypeFields rt

            icases <- CM.mapM inferFieldType cases
            (idef, defcon) <- case def of
              Nothing -> pure (Nothing, [])
              Just d -> do
                idf <- infer d
                return (Just idf, termConstraints idf)
            let innerConstraints = L.concat (defcon:(termConstraints . fieldTerm <$> icases))

            let idoms = termType . fieldTerm <$> icases
            let sdoms = fieldTypeType <$> sfields
            cod <- freshName
            let outerConstraints = L.zipWith (\t d -> (t, Types.function d cod)) idoms sdoms

            yieldElimination (EliminationUnion (CaseStatement name idef icases))
              (Types.function (TypeUnion rt) cod)
              (innerConstraints ++ outerConstraints)

        EliminationWrap name -> do
          typ <- withGraphContext $ requireWrappedType name
          yieldElimination (EliminationWrap name) (Types.function (Types.wrap name) typ) []

      FunctionLambda (Lambda v body) -> do
        tv <- freshName
        i <- extendEnvironment v (monotype tv) $ infer body
        yieldFunction (FunctionLambda $ Lambda v i) (Types.function tv (termType i)) (termConstraints i)

      FunctionPrimitive name -> do
        t <- withGraphContext $ typeOfPrimitive name
        yieldFunction (FunctionPrimitive name) t []

    TermLet lt -> withLet lt pure

    TermList els -> do
        v <- freshName
        if L.null els
          then yield (TermList []) (Types.list v) []
          else do
            iels <- CM.mapM infer els
            let co = (\e -> (v, termType e)) <$> iels
            let ci = L.concat (termConstraints <$> iels)
            yield (TermList iels) (Types.list v) (co ++ ci)

    TermLiteral l -> yield (TermLiteral l) (Types.literal $ literalType l) []

    TermMap m -> do
        kv <- freshName
        vv <- freshName
        if M.null m
          then yield (TermMap M.empty) (Types.map kv vv) []
          else do
            pairs <- CM.mapM toPair $ M.toList m
            let co = L.concat ((\(k, v) -> [(kv, termType k), (vv, termType v)]) <$> pairs)
            let ci = L.concat ((\(k, v) -> termConstraints k ++ termConstraints v) <$> pairs)
            yield (TermMap $ M.fromList pairs) (Types.map kv vv) (co ++ ci)
      where
        toPair (k, v) = do
          ik <- infer k
          iv <- infer v
          return (ik, iv)

    TermOptional m -> do
      v <- freshName
      case m of
        Nothing -> yield (TermOptional Nothing) (Types.optional v) []
        Just e -> do
          i <- infer e
          let ci = termConstraints i
          yield (TermOptional $ Just i) (Types.optional v) ((v, termType i):ci)

    TermProduct tuple -> do
      is <- CM.mapM infer tuple
      yield (TermProduct is) (TypeProduct $ fmap termType is) (L.concat $ fmap termConstraints is)

    TermRecord (Record n fields) -> do
        rt <- withGraphContext $ requireRecordType True n
        ifields <- CM.mapM inferFieldType fields
        let ci = L.concat (termConstraints . fieldTerm <$> ifields)
        let irt = TypeRecord $ RowType n Nothing (fieldType <$> ifields)
        yield (TermRecord $ Record n ifields) irt ((TypeRecord rt, irt):ci)

    TermSet els -> do
      v <- freshName
      if S.null els
        then yield (TermSet S.empty) (Types.set v) []
        else do
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
          else freshName

    TermUnion (Injection n field) -> do
        rt <- withGraphContext $ requireUnionType True n
        sfield <- findMatchingField (fieldName field) (rowTypeFields rt)
        ifield <- inferFieldType field
        let ci = termConstraints $ fieldTerm ifield
        let co = (termType $ fieldTerm ifield, fieldTypeType sfield)
        yield (TermUnion $ Injection n ifield) (TypeUnion rt) (co:ci)

    TermVariable v -> do
      t <- requireName v
      yield (TermVariable v) t []

    TermWrap (Nominal name term1) -> do
      typ <- withGraphContext $ requireWrappedType name
      i <- infer term1
      yield (TermWrap $ Nominal name i) (Types.wrap name) (termConstraints i ++ [(typ, termType i)])

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: Show a => Term a -> GraphFlow a (Type a)
decodeStructuralType term = do
  typ <- epsilonDecodeType term
  let typ' = stripType typ
  case typ' of
    TypeWrap name -> withSchemaContext $ withTrace "decode structural type" $ do
      el <- requireElement name
      decodeStructuralType $ elementData el
    _ -> pure typ

extendEnvironment :: Name -> TypeScheme a -> Flow (InferenceContext a) x -> Flow (InferenceContext a) x
extendEnvironment n sc = withEnvironment (\e -> M.insert n sc $ M.delete n e)

fieldType :: Field (InfAnn a) -> FieldType a
fieldType (Field fname term) = FieldType fname $ termType term

findMatchingField :: Show a => FieldName -> [FieldType a] -> Flow (InferenceContext a) (FieldType a)
findMatchingField fname sfields = case L.filter (\f -> fieldTypeName f == fname) sfields of
  []    -> fail $ "no such field: " ++ unFieldName fname
  (h:_) -> return h

freshName :: Flow (InferenceContext a) (Type a)
freshName = do
    InferenceContext cx s e <- getState
    putState $ InferenceContext cx (s + 1) e
    return $ Types.variable (unName $ normalVariables !! s)

generalize :: Show a => TypingEnvironment a -> Type a -> TypeScheme a
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

infer :: (Ord a, Show a) => Term a -> Flow (InferenceContext a) (Term (InfAnn a))
infer term = do
  g <- inferenceContextGraph <$> getState
  mt <- withGraphContext $ annotationClassTermType (graphAnnotations g) term
  case mt of
    Just typ -> do
      i <- applyRules term
      return $ TermAnnotated $ Annotated i (termMeta g term, typ, []) -- TODO: unify "suggested" types with inferred types
    Nothing -> applyRules term

inferFieldType :: (Ord a, Show a) => Field a -> Flow (InferenceContext a) (Field (InfAnn a))
inferFieldType (Field fname term) = Field fname <$> infer term

instantiate :: TypeScheme a -> Flow (InferenceContext a) (Type a)
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshName) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

monotype :: Type a -> TypeScheme a
monotype typ = TypeScheme [] typ

reduceType :: (Ord a, Show a) => Type a -> Type a
reduceType t = t -- betaReduceType cx t

requireName :: Show a => Name -> Flow (InferenceContext a) (Type a)
requireName v = do
  env <- inferenceContextEnvironment <$> getState
  case M.lookup v env of
    Nothing -> fail $ "variable not bound in environment: " ++ unName v
    Just s  -> instantiate s

termConstraints :: Term (InfAnn a) -> [Constraint a]
termConstraints (TermAnnotated (Annotated _ (_, _, constraints))) = constraints

termType :: Term (InfAnn a) -> Type a
termType (TermAnnotated (Annotated _ (_, typ, _))) = typ

typeOfElement :: Show a => Name -> GraphFlow a (Type a)
typeOfElement name = withTrace "type of element" $ do
  el <- requireElement name
  decodeStructuralType $ elementSchema el

typeOfPrimitive :: Name -> GraphFlow a (Type a)
typeOfPrimitive name = primitiveType <$> requirePrimitive name

typeOfTerm :: Term a -> GraphFlow a (Maybe (Type a))
typeOfTerm term = do
  anns <- graphAnnotations <$> getState
  annotationClassTypeOf anns $ annotationClassTermAnnotation anns term

withEnvironment :: (TypingEnvironment a -> TypingEnvironment a) -> Flow (InferenceContext a) x -> Flow (InferenceContext a) x
withEnvironment m f = do
  InferenceContext cx i e <- getState
  withState (InferenceContext cx i (m e)) f

withGraphContext :: GraphFlow a x -> Flow (InferenceContext a) x
withGraphContext f = do
  cx <- inferenceContextGraph <$> getState
  withState cx f

withLet :: (Ord a, Show a) => Let a -> (Term (InfAnn a) -> Flow (InferenceContext a) x) -> Flow (InferenceContext a) x
withLet (Let bindings env) flow = withTrace ("let(" ++ L.intercalate "," (unName . fst <$> M.toList bindings) ++ ")") $ do
    state0 <- getState
    e <- preExtendEnv bindings $ inferenceContextEnvironment state0
    let state = state0 {inferenceContextEnvironment = e}
    withState state $ do
      -- TODO: perform a topologic sort on these bindings; this process should be unified with that of elements in a graph
      let bl = M.toList bindings

      is <- CM.mapM infer (snd <$> bl) -- TODO
      let tc = L.concat (termConstraints <$> is)
      sub <- withGraphContext $ solveConstraints tc
      let ts = (reduceType . substituteInType sub . termType) <$> is
      let te = M.map (substituteInScheme sub) $ inferenceContextEnvironment state
      let sc = generalize te <$> ts

      let tenv = withEnvironment (M.map (substituteInScheme sub)) $ infer env
      i2 <- L.foldl (\t (x, s) -> extendEnvironment x s t) tenv $ L.zip (fst <$> bl) sc

      let t2 = termType i2
      let c2 = termConstraints i2

      let ibindings = L.zip (fst <$> bl) is
      result <- yield (TermLet $ Let (M.fromList ibindings) i2) t2 (tc ++ c2)

      let state1 = state {
            inferenceContextEnvironment = extendEnv ibindings te,
            inferenceContextGraph = extendGraph ibindings $ inferenceContextGraph state}
      withState state1 $ flow result
  where
    fsti (x, _, _) = x
    sndi (_, x, _) = x
    extendEnv bindings e = L.foldl addType e bindings
      where
        addType e (name, infterm) = M.insert name ts e
          where
            ts = monotype $ case infterm of -- TODO: assuming monotypes
              TermAnnotated (Annotated _ ann) -> sndi ann
    extendGraph bindings g = g {graphElements = L.foldl addElement (graphElements g) bindings}
      where
        addElement m (name, infterm) = M.insert name (Element name sch dat) m
          where
            sch = epsilonEncodeType $ case infterm of
              TermAnnotated (Annotated _ ann) -> sndi ann
            dat = rewriteTermMeta fsti infterm
    -- Add any manual type annotations for the bindings to the environment, enabling type inference over recursive definitions
    preExtendEnv bindings e = withGraphContext $ CM.foldM addPair e $ M.toList bindings
      where
        addPair e (name, term) = do
          mtyp <- typeOfTerm term
          return $ case mtyp of
            Nothing -> e
            Just typ -> M.insert name (monotype typ) e

yield :: Term (InfAnn a) -> Type a -> [Constraint a] -> Flow (InferenceContext a) (Term (InfAnn a))
yield term typ constraints = do
  case term of
    TermAnnotated _ -> fail "doubly-annotated term"
    _ -> pure ()
  g <- inferenceContextGraph <$> getState
  return $ TermAnnotated $ Annotated term (annotationClassDefault $ graphAnnotations g, typ, constraints)

yieldFunction :: Function (InfAnn a) -> Type a -> [Constraint a] -> Flow (InferenceContext a) (Term (InfAnn a))
yieldFunction fun = yield (TermFunction fun)

yieldElimination :: Elimination (InfAnn a) -> Type a -> [Constraint a] -> Flow (InferenceContext a) (Term (InfAnn a))
yieldElimination e = yield (TermFunction $ FunctionElimination e)
