-- | Inference rules

module Hydra.Types.Rules where

import Hydra.Basics
import Hydra.Common
import Hydra.Compute
import Hydra.Core
import Hydra.CoreDecoding
import Hydra.Graph
import Hydra.Lexical
import Hydra.Mantle
import Hydra.Monads
import Hydra.Rewriting
import Hydra.Types.Substitution
import Hydra.Types.Unification
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


data InferenceContext m = InferenceContext {
  inferenceContextGraph :: Context m,
  inferenceContextCounter :: Int,
  inferenceContextEnviroment :: TypingEnvironment m}

type TypingEnvironment m = M.Map Name (TypeScheme m)

applyRules :: (Ord m, Show m) => Term m -> Flow (InferenceContext m) (Term (m, Type m, [Constraint m]))
applyRules term = case term of
    TermAnnotated (Annotated term1 ann) -> do
      iterm <- infer term1
      return $ case iterm of
        -- `yield` produces the default annotation, which can just be replaced
        TermAnnotated (Annotated trm (_, t, c)) -> TermAnnotated (Annotated trm (ann, t, c))

    TermApplication (Application fun arg) -> do
      ifun <- infer fun
      iarg <- infer arg
      v <- freshName
      let c = (termConstraints ifun) ++ (termConstraints iarg) ++ [(termType ifun, Types.function (termType iarg) v)]
      let app = TermApplication $ Application ifun iarg
      yield app v c

    TermElement name -> do
      et <- withGraphContext $ typeOfElement name
      yield (TermElement name) (Types.element et) []

    TermFunction f -> case f of

      FunctionElimination e -> case e of

        EliminationElement -> do
          et <- freshName
          yieldElimination EliminationElement (Types.function (Types.element et) et) []

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

        EliminationUnion (CaseStatement name cases) -> do
            rt <- withGraphContext $ requireUnionType True name
            let sfields = rowTypeFields rt

            icases <- CM.mapM inferFieldType cases
            let innerConstraints = L.concat (termConstraints . fieldTerm <$> icases)

            let idoms = termType . fieldTerm <$> icases
            let sdoms = fieldTypeType <$> sfields
            cod <- freshName
            let outerConstraints = L.zipWith (\t d -> (t, Types.function d cod)) idoms sdoms

            yieldElimination (EliminationUnion (CaseStatement name icases))
              (Types.function (TypeUnion rt) cod)
              (innerConstraints ++ outerConstraints)

        EliminationWrap name -> do
          typ <- withGraphContext $ requireWrappedType name
          yieldElimination (EliminationWrap name) (Types.function (Types.wrap name) typ) []

      FunctionLambda (Lambda v body) -> do
        tv <- freshName
        i <- extendEnvironment v (TypeScheme [] tv) $ infer body
        yieldFunction (FunctionLambda $ Lambda v i) (Types.function tv (termType i)) (termConstraints i)

      FunctionPrimitive name -> do
        t <- withGraphContext $ typeOfPrimitive name
        yieldFunction (FunctionPrimitive name) t []

    TermLet (Let bindings env) -> do
        cs <- inferenceContextEnviroment <$> getState

        let bl = M.toList bindings
        is <- CM.mapM infer (snd <$> bl)
        let tc = L.concat (termConstraints <$> is)
        sub <- withGraphContext $ solveConstraints tc
        let ts = (reduceType . substituteInType sub . termType) <$> is
        let te = M.map (substituteInScheme sub) cs
        let sc = generalize te <$> ts

        let tenv = withEnvironment (M.map (substituteInScheme sub)) $ infer env
        i2 <- L.foldl (\t (x, s) -> extendEnvironment x s t) tenv $ L.zip (fst <$> bl) sc

        let t2 = termType i2
        let c2 = termConstraints i2

        yield (TermLet $ Let (M.fromList $ L.zip (fst <$> bl) is) i2) t2 (tc ++ c2)
 
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
  where     
    yieldFunction fun = yield (TermFunction fun)

    yieldElimination e = yield (TermFunction $ FunctionElimination e)

    yield term typ constraints = do
      cx <- inferenceContextGraph <$> getState
      return $ TermAnnotated $ Annotated term (annotationClassDefault $ contextAnnotations cx, typ, constraints)

-- Decode a type, eliminating nominal types for the sake of unification
decodeStructuralType :: Show m => Term m -> GraphFlow m (Type m)
decodeStructuralType term = do
  typ <- epsilonDecodeType term
  let typ' = stripType typ
  case typ' of
    TypeWrap name -> withSchemaContext $ withTrace "decode structural type" $ do
      el <- requireElement name
      decodeStructuralType $ elementData el
    _ -> pure typ

extendEnvironment :: Name -> TypeScheme m -> Flow (InferenceContext m) a -> Flow (InferenceContext m) a
extendEnvironment x sc = withEnvironment (\e -> M.insert x sc $ M.delete x e)

fieldType :: Field (m, Type m, [Constraint m]) -> FieldType m
fieldType (Field fname term) = FieldType fname $ termType term

findMatchingField :: Show m => FieldName -> [FieldType m] -> Flow (InferenceContext m) (FieldType m)
findMatchingField fname sfields = case L.filter (\f -> fieldTypeName f == fname) sfields of
  []    -> fail $ "no such field: " ++ unFieldName fname
  (h:_) -> return h

freshName :: Flow (InferenceContext m) (Type m)
freshName = do
    InferenceContext cx s e <- getState
    putState $ InferenceContext cx (s + 1) e
    return $ Types.variable (unName $ normalVariables !! s)

generalize :: Show m => TypingEnvironment m -> Type m -> TypeScheme m
generalize env t  = TypeScheme vars t
  where
    vars = S.toList $ S.difference
      (freeVariablesInType t)
      (L.foldr (S.union . freeVariablesInScheme) S.empty $ M.elems env)

infer :: (Ord m, Show m) => Term m -> Flow (InferenceContext m) (Term (m, Type m, [Constraint m]))
infer term = do
  cx <- inferenceContextGraph <$> getState
  mt <- withGraphContext $ annotationClassTermType (contextAnnotations cx) term
  case mt of
    Just typ -> do
      i <- applyRules term
      return $ TermAnnotated $ Annotated i (termMeta cx term, typ, []) -- TODO: unify "suggested" types with inferred types
    Nothing -> applyRules term

inferFieldType :: (Ord m, Show m) => Field m -> Flow (InferenceContext m) (Field (m, Type m, [Constraint m]))
inferFieldType (Field fname term) = Field fname <$> infer term

instantiate :: TypeScheme m -> Flow (InferenceContext m) (Type m)
instantiate (TypeScheme vars t) = do
    vars1 <- mapM (const freshName) vars
    return $ substituteInType (M.fromList $ zip vars vars1) t

reduceType :: (Ord m, Show m) => Type m -> Type m
reduceType t = t -- betaReduceType cx t

requireName :: Show m => Name -> Flow (InferenceContext m) (Type m)
requireName v = do
  env <- inferenceContextEnviroment <$> getState
  case M.lookup v env of
    Nothing -> fail $ "variable not bound in environment: " ++ unName v
    Just s  -> instantiate s

termConstraints :: Term (m, Type m, [Constraint m]) -> [Constraint m]
termConstraints (TermAnnotated (Annotated _ (_, _, constraints))) = constraints

termType :: Term (m, Type m, [Constraint m]) -> Type m
termType (TermAnnotated (Annotated _ (_, typ, _))) = typ

typeOfElement :: Show m => Name -> GraphFlow m (Type m)
typeOfElement name = withTrace "type of element" $ do
  el <- requireElement name
  decodeStructuralType $ elementSchema el

typeOfPrimitive :: Name -> GraphFlow m (Type m)
typeOfPrimitive name = primitiveType <$> requirePrimitive name

withEnvironment :: (TypingEnvironment m -> TypingEnvironment m) -> Flow (InferenceContext m) a -> Flow (InferenceContext m) a
withEnvironment m f = do
  InferenceContext cx i e <- getState
  withState (InferenceContext cx i (m e)) f

withGraphContext :: GraphFlow m a -> Flow (InferenceContext m) a
withGraphContext f = do
  cx <- inferenceContextGraph <$> getState
  withState cx f
