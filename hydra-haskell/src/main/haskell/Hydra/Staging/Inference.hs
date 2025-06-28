module Hydra.Staging.Inference (
  module Hydra.Inference,
  module Hydra.Staging.Inference,
) where

import Hydra.Inference
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Flows as Flows
import qualified Hydra.Functions as Functions
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Flows as LibFlows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Strip as Strip
import qualified Hydra.Substitution as Substitution
import qualified Hydra.Typing as Typing
import qualified Hydra.Unification as Unification
import qualified Hydra.Variants as Variants
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Extract.Core as ExtractCore

import qualified Control.Monad as CM
import qualified Data.Either   as E
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Maybe    as Y


-- Disable type checking by default, for better performance.
debugInference = True

--------------------------------------------------------------------------------
-- Variables

key_vcount = Core.Name "inferenceTypeVariableCount"

freshName :: Compute.Flow s Core.Name
freshName = Inference.normalTypeVariable <$> Annotations.nextCount key_vcount

freshNames :: Int -> Compute.Flow s [Core.Name]
freshNames n = LibFlows.sequence $ L.replicate n freshName

freshVariableType :: Compute.Flow s Core.Type
freshVariableType = Core.TypeVariable <$> freshName

--------------------------------------------------------------------------------
-- Type checking

typeOf :: Typing.InferenceContext -> S.Set Core.Name -> M.Map Core.Name Core.Type -> Core.Term -> Compute.Flow s Core.Type
--typeOf cx vars types term = case term of
typeOf cx vars types term = Flows.withTrace ("checking type of: " ++ ShowCore.term term ++ " (vars: " ++ show (fmap Core.unName $ S.toList vars) ++ ", types: " ++ (show types) ++ ")") $ case term of
    Core.TermAnnotated (Core.AnnotatedTerm term1 _) -> typeOf cx vars types term1
    Core.TermApplication (Core.Application a b) -> do
      t1 <- typeOf cx vars types a
      t2 <- typeOf cx vars types b
      checkTypeVariables vars t1
      checkTypeVariables vars t2
      case t1 of
        Core.TypeFunction (Core.FunctionType p q) -> if p == t2
          then return q
          else LibFlows.fail $ "expected " ++ ShowCore.type_ p ++ " in " ++ ShowCore.term term ++ " but found " ++ ShowCore.type_ t2
        _ -> LibFlows.fail $ "left hand side of application " ++ ShowCore.term term ++ " is not a function type: " ++ ShowCore.type_ t1
    Core.TermFunction f -> case f of
      Core.FunctionElimination elm -> case elm of
        Core.EliminationProduct (Core.TupleProjection index arity mtypes) -> case mtypes of
          Nothing -> LibFlows.fail $ "untyped tuple projection: " ++ ShowCore.term term
          Just types -> do
            CM.mapM (checkTypeVariables vars) types
            return $ Types.function (Types.product types) (types !! index)
--        EliminationRecord (Projection tname (Field fname fterm)) -> ...
--        EliminationUnion (CaseStatement tname def cases) -> ...
--        EliminationWrap tname -> ...
      Core.FunctionLambda (Core.Lambda x mt e) -> case mt of
        Nothing -> LibFlows.fail $ "untyped lambda: " ++ ShowCore.term term
        Just t -> do
          checkTypeVariables vars t
          t1 <- typeOf cx vars (M.insert x t types) e
          checkTypeVariables vars t1
          return $ Types.function t t1
      Core.FunctionPrimitive name -> typeSchemeToFType <$> ts
        where
          -- Note: no instantiation
          ts = case M.lookup name (Typing.inferenceContextPrimitiveTypes cx) of
            Nothing -> LibFlows.fail $ "no such primitive: " ++ Core.unName name
            Just ts0 -> return ts0
    Core.TermLet (Core.Let es e) -> do
        btypes <- CM.mapM binType es
        let types2 = M.union (M.fromList $ L.zip bnames btypes) types
        est <- CM.mapM (\v -> typeOf cx vars types2 v) bterms
        CM.mapM (checkTypeVariables vars) est
        CM.mapM (checkTypeVariables vars) btypes
        if est == btypes
          then typeOf cx vars types2 e
          else LibFlows.fail $ "binding types disagree: " ++ show (fmap ShowCore.type_ est) ++ " and " ++ show (fmap ShowCore.type_ btypes)
      where
        bnames = fmap Core.letBindingName es
        bterms = fmap Core.letBindingTerm es
        binType b = case Core.letBindingType b of
          Nothing -> LibFlows.fail $ "untyped let binding in " ++ ShowCore.term term
          Just ts -> return $ typeSchemeToFType ts
    Core.TermList els -> case els of
      [] -> do
        t <- freshName
        let var = Core.TypeVariable t
        return $ Core.TypeForall $ Core.ForallType t (Core.TypeList var)
      (x:xs) -> do
        tx <- typeOf cx vars types x >>= instantiateFType -- TODO: is instantiation really necessary?
        CM.forM_ xs $ \e -> do
          t <- typeOf cx vars types e >>= instantiateFType -- TODO: is instantiation really necessary?
          Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) t tx "type check over collection"
        checkTypeVariables vars tx
        return $ Core.TypeList tx
    Core.TermLiteral lit -> return $ Core.TypeLiteral $ Variants.literalType lit
    Core.TermMap m -> if M.null m
        then return $ typeSchemeToFType $ Types.poly ["k", "v"] $ Types.map (Types.var "k") (Types.var "v")
        else do
          kt <- (CM.mapM (typeOf cx vars types) $ fmap fst pairs) >>= singleType "map keys"
          vt <- (CM.mapM (typeOf cx vars types) $ fmap snd pairs) >>= singleType "map values"
          checkTypeVariables vars kt
          checkTypeVariables vars vt
          return $ Core.TypeMap $ Core.MapType kt vt
      where
        pairs = M.toList m
    Core.TermOptional mt -> typeOfCollection cx "optional" Core.TypeOptional vars types $ Y.maybe [] (\x -> [x]) mt
    Core.TermProduct tuple -> do
      etypes <- CM.mapM (typeOf cx vars types) tuple
      CM.mapM (checkTypeVariables vars) etypes
      return $ Core.TypeProduct etypes

    Core.TermRecord (Core.Record tname fields) -> do
      ftypes <- CM.mapM (typeOf cx vars types) $ fmap Core.fieldTerm fields
      CM.mapM (checkTypeVariables vars) ftypes
      typeOfNominal "record typeOf" cx tname $ Core.TypeRecord $ Core.RowType tname $ L.zipWith Core.FieldType (fmap Core.fieldName fields) ftypes

    Core.TermSet els -> typeOfCollection cx "set" Core.TypeSet vars types $ S.toList els
--    TermSum (Sum idx size term1) -> ...
    Core.TermTypeAbstraction (Core.TypeAbstraction v e) -> do
      t1 <- typeOf cx (S.insert v vars) types e
      checkTypeVariables (S.insert v vars) t1
      return $ Core.TypeForall $ Core.ForallType v t1
    Core.TermTypeApplication (Core.TypedTerm e t) -> do
      t1 <- typeOf cx vars types e
      checkTypeVariables vars t1
--      Flows.fail $ "type-checking type application"
--        ++ "\n\tterm: " ++ ShowCore.term e
--        ++ "\n\ttype: " ++ ShowCore.type_ t
--        ++ "\n\tvars: " ++ show (fmap unName $ S.toList vars)
--        ++ "\n\tt1: " ++ ShowCore.type_ t1
      case t1 of
        Core.TypeForall (Core.ForallType v t2) -> return $ Substitution.substInType (Typing.TypeSubst $ M.fromList [(v, t)]) t2
        _ -> LibFlows.fail $ "not a forall type: " ++ ShowCore.type_ t1 ++ " in " ++ ShowCore.term term
    Core.TermUnion (Core.Injection tname (Core.Field fname term1)) -> do
        ftype <- typeOf cx vars types term1
        checkTypeVariables vars ftype

        Core.TypeScheme svars styp <- requireSchemaType cx tname
        sfields <- ExtractCore.unionType tname styp
        let fnames = fmap Core.fieldTypeName sfields
        ftypes <- CM.mapM (fieldTypeOf ftype) fnames

        let expected = Core.TypeUnion $ Core.RowType tname $ L.zipWith Core.FieldType fnames ftypes
        (Typing.TypeSubst subst) <- Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) styp expected "union typeOf"

        let tparams = fmap (resolveType subst) svars
        return $ nominalApplication tname tparams
      where
        fieldTypeOf ftype fname1 = if fname1 == fname
          then return ftype
          else Core.TypeVariable <$> freshName
        resolveType subst v = Y.fromMaybe (Core.TypeVariable v) $ M.lookup v subst

    Core.TermVariable name -> case M.lookup name types of
      Nothing -> LibFlows.fail $ "unbound variable: " ++ Core.unName name
      Just t -> return t

    Core.TermWrap (Core.WrappedTerm tname innerTerm) -> do
      innerType <- typeOf cx vars types innerTerm
      checkTypeVariables vars innerType
      typeOfNominal "wrapper typeOf" cx tname $ Core.TypeWrap $ Core.WrappedType tname innerType

    _ -> LibFlows.fail $ "unsupported term variant in typeOf: " ++ show (Variants.termVariant term)

typeOfCollection :: Typing.InferenceContext -> String -> (Core.Type -> Core.Type) -> S.Set Core.Name -> M.Map Core.Name Core.Type -> [Core.Term] -> Compute.Flow s Core.Type
typeOfCollection cx desc cons vars types els = if L.null els
  then return $ typeSchemeToFType $ Types.poly ["t"] $ cons $ Types.var "t"
  else do
    et <- CM.mapM (typeOf cx vars types) els >>= singleType desc
    checkTypeVariables vars et
    return $ cons et

typeOfNominal :: String -> Typing.InferenceContext -> Core.Name -> Core.Type -> Compute.Flow s Core.Type
typeOfNominal desc cx tname expected = do
    Core.TypeScheme svars styp <- requireSchemaType cx tname

--      Flows.fail $ "svars: " ++ show svars ++ ", styp: " ++ ShowCore.type_ styp
--      Flows.fail $ "expected: " ++ ShowCore.type_ expected ++ ", schema type: " ++ ShowCore.type_ styp

    (Typing.TypeSubst subst) <- Unification.unifyTypes (Typing.inferenceContextSchemaTypes cx) styp expected desc

    let tparams = fmap (resolveType subst) svars
    return $ nominalApplication tname tparams
  where
    resolveType subst v = Y.fromMaybe (Core.TypeVariable v) $ M.lookup v subst

singleType :: String -> [Core.Type] -> Compute.Flow s Core.Type
singleType desc types = if (L.foldl (\b t -> b && t == h) True types)
    then return h
    else LibFlows.fail $ "unequal types " ++ show (fmap ShowCore.type_ types) ++ " in " ++ desc
  where
    h = L.head types

checkType :: S.Set Core.Name -> Typing.InferenceContext -> Core.Type -> Core.Term -> Compute.Flow s ()
checkType k g t e = if debugInference
  then do
    t0 <- typeOf g k (toFContext g) e
    if t0 == t
      then return ()
      else LibFlows.fail $ "type checking failed: expected " ++ ShowCore.type_ t ++ " but found " ++ ShowCore.type_ t0
  else return ()

checkTypeVariables :: S.Set Core.Name -> Core.Type -> Compute.Flow s ()
checkTypeVariables vars typ = case typ of
  Core.TypeForall (Core.ForallType v body) -> checkTypeVariables (S.insert v vars) body
  Core.TypeVariable v -> if S.member v vars
    then return ()
    else LibFlows.fail $ "unbound type variable \"" ++ Core.unName v ++ "\" in " ++ ShowCore.type_ typ
  _ -> do
    CM.sequence $ fmap (checkTypeVariables vars) $ Rewriting.subtypes typ
    return ()

typeSchemeToFType :: Core.TypeScheme -> Core.Type
typeSchemeToFType (Core.TypeScheme vars body) = L.foldl (\t v -> Core.TypeForall $ Core.ForallType v t) body $ L.reverse vars

toFContext :: Typing.InferenceContext -> M.Map Core.Name Core.Type
toFContext = fmap typeSchemeToFType . Typing.inferenceContextDataTypes

--------------------------------------------------------------------------------
-- Inference

showInferenceResult :: Typing.InferenceResult -> String
showInferenceResult (Typing.InferenceResult term typ subst) = "{"
    ++ "term=" ++ ShowCore.term term ++ ", "
    ++ "type= " ++ ShowCore.type_ typ ++ ", "
    ++ "subst= " ++ ShowCore.typeSubst subst ++ "}"

freeVariablesInContext :: Typing.InferenceContext -> S.Set Core.Name
freeVariablesInContext cx = L.foldl S.union S.empty $ fmap Rewriting.freeVariablesInTypeSchemeSimple $ M.elems $ Typing.inferenceContextDataTypes cx

generalize :: Typing.InferenceContext -> Core.Type -> Core.TypeScheme
generalize cx typ = Core.TypeScheme vars typ
  where
    vars = L.nub $ L.filter isUnbound $ S.toList $ Rewriting.freeVariablesInType typ
    isUnbound v = not (S.member v $ freeVariablesInContext cx)
      && not (M.member v $ Typing.inferenceContextSchemaTypes cx)

graphToInferenceContext :: Graph.Graph -> Compute.Flow s Typing.InferenceContext
graphToInferenceContext g0 = do
    schemaTypes <- Schemas.schemaGraphToTypingEnvironment schema
    return $ Typing.InferenceContext schemaTypes primTypes varTypes False
  where
    schema = Y.fromMaybe g0 $ Graph.graphSchema g0
    primTypes = M.fromList $ fmap (\p -> (Graph.primitiveName p, Graph.primitiveType p)) (M.elems $ Graph.graphPrimitives g0)
    varTypes = M.empty

-- Note: this operation is expensive, as it creates a new typing environment for each individual term
inferInGraphContext :: Core.Term -> Compute.Flow Graph.Graph Typing.InferenceResult
inferInGraphContext term = do
  g <- Errors.getState
  cx <- graphToInferenceContext g
  inferTypeOfTerm cx term "single term"

inferGraphTypes :: Graph.Graph -> Compute.Flow s Graph.Graph
inferGraphTypes g0 = Flows.withTrace "graph inference" $ do
    cx <- graphToInferenceContext g0
    Flows.bind (inferTypeOfTerm cx (toLetTerm g0) "graph term") withResult
  where
    toLetTerm g = Core.TermLet $ Core.Let (fmap toBinding $ M.elems $ Graph.graphElements g) $ Graph.graphBody g
      where
        toBinding (Graph.Element name term _) = Core.LetBinding name term Nothing
    withResult (Typing.InferenceResult term ts _) = case Rewriting.normalizeTypeVariablesInTerm term of
      Core.TermLet l -> LibFlows.pure $ fromLetTerm l
      _ -> LibFlows.fail $ "Expected inferred graph as let term"
    fromLetTerm (Core.Let bindings env) = g0 {
        Graph.graphElements = M.fromList $ fmap fromBinding bindings,
        Graph.graphBody = env,
        Graph.graphEnvironment = M.empty,
        Graph.graphTypes = M.empty}
      where
        fromBinding (Core.LetBinding name term mt) = (name, Graph.Element name term mt)

inferMany :: Typing.InferenceContext -> [(Core.Term, String)] -> Compute.Flow s ([Core.Term], [Core.Type], Typing.TypeSubst)
inferMany cx pairs = case pairs of
  [] -> return ([], [], Substitution.idTypeSubst)
  ((e, desc):tl) -> do
    (Typing.InferenceResult e1 t1 s1) <- inferTypeOfTerm cx e desc
    (e2, t2, s2) <- inferMany (Substitution.substInContext s1 cx) tl
    return (
      (Substitution.substTypesInTerm s2 e1):e2,
      (Substitution.substInType s2 t1):t2,
      Substitution.composeTypeSubst s1 s2)

inferTwo :: Typing.InferenceContext -> Core.Term -> String -> Core.Term -> String -> Compute.Flow s (Core.Term, Core.Type, Core.Term, Core.Type, Typing.TypeSubst)
inferTwo cx term1 desc1 term2 desc2 = Flows.map withResult $ inferMany cx [(term1, desc1), (term2, desc2)]
  where
    withResult ([e1, e2], [t1, t2], s) = (e1, t1, e2, t2, s)
    withResult _ = error "unexpected result from inferMany"

inferTypeOf :: Typing.InferenceContext -> Core.Term -> Compute.Flow s (Core.Term, Core.TypeScheme)
inferTypeOf cx term = bindInferredTerm cx letTerm "infer type of term" unifyAndSubst
  where
    letTerm = Core.TermLet $ Core.Let [Core.LetBinding (Core.Name "ignoredVariableName") term Nothing] $ Terms.string "ignoredEnvironment"
    unifyAndSubst result = do
        (Core.Let bindings _) <- Annotations.withEmptyGraph $ ExtractCore.letTerm $ Rewriting.normalizeTypeVariablesInTerm $ Typing.inferenceResultTerm result
        case bindings of
          [Core.LetBinding _ term1 (Just ts)] -> return (term1, ts)
          _ -> LibFlows.fail $ "Expected a single binding with a type scheme, but got: " ++ show bindings
      where
        subst = Typing.inferenceResultSubst result

inferTypeOfAnnotatedTerm :: Typing.InferenceContext -> Core.AnnotatedTerm -> Compute.Flow s Typing.InferenceResult
inferTypeOfAnnotatedTerm cx (Core.AnnotatedTerm term ann) = Flows.map withResult $ inferTypeOfTerm cx term "annotated term"
  where
    withResult (Typing.InferenceResult iterm itype isubst)
      = Typing.InferenceResult (Core.TermAnnotated $ Core.AnnotatedTerm iterm ann) itype isubst

inferTypeOfApplication :: Typing.InferenceContext -> Core.Application -> Compute.Flow s Typing.InferenceResult
inferTypeOfApplication cx (Core.Application e0 e1) = bindInferredTerm cx e0 "lhs" withLhs
  where
    withLhs (Typing.InferenceResult a t0 s0) = bindInferredTerm (Substitution.substInContext s0 cx) e1 "rhs" withRhs
      where
        withRhs (Typing.InferenceResult b t1 s1) = bindVar withVar
          where
            withVar v = Flows.map withSubst $ Unification.unifyTypes
                (Typing.inferenceContextSchemaTypes cx)
                (Substitution.substInType s1 t0)
                (Types.function t1 $ Core.TypeVariable v)
                "application lhs"
              where
                withSubst s2 = Typing.InferenceResult rExpr rType rSubst
                  where
                    rExpr = Terms.apply (Substitution.substTypesInTerm (Substitution.composeTypeSubst s1 s2) a) (Substitution.substTypesInTerm s2 b)
                    rType = Substitution.substInType s2 $ Core.TypeVariable v
                    rSubst = Substitution.composeTypeSubstList [s0, s1, s2]

inferTypeOfCaseStatement :: Typing.InferenceContext -> Core.CaseStatement -> Compute.Flow s Typing.InferenceResult
inferTypeOfCaseStatement cx (Core.CaseStatement tname dflt cases) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    fnames = fmap Core.fieldName cases
    withSchemaType (Core.TypeScheme svars styp) = Flows.bind (ExtractCore.unionType tname styp) withFields
      where
        withFields sfields = Flows.bind2
           (traverse (\t -> inferTypeOfTerm cx t $ "case " ++ Core.unName tname ++ ".<default>") dflt)
           (inferMany cx $ fmap (\f -> (Core.fieldTerm f, "case " ++ Core.unName tname ++ "." ++ Core.unName (Core.fieldName f))) cases) withResults
          where
            withResults mr (iterms, itypes, isubst) = bindVar withCod
              where
                withCod codv = mapConstraints cx withConstraints (dfltConstraints ++ caseConstraints)
                  where
                    cod = Core.TypeVariable codv
                    dfltConstraints = Functions.optionalToList $ fmap (\r -> Typing.TypeConstraint cod (Typing.inferenceResultType r) "match default") mr
                    caseConstraints = Y.catMaybes $ L.zipWith (\fname itype -> fmap (\r -> toConstraint r itype) $ M.lookup fname caseMap) fnames itypes
                      where
                        caseMap = M.fromList $ fmap (\(Core.FieldType fname ftype) -> (fname, ftype)) sfields
                        toConstraint ftyp r = Typing.TypeConstraint r (Types.function ftyp cod) "case type"
                    withConstraints subst = yield
                        (Core.TermFunction $ Core.FunctionElimination $ Core.EliminationUnion $ Core.CaseStatement tname (fmap Typing.inferenceResultTerm mr) $ L.zipWith Core.Field fnames iterms)
                        (Core.TypeFunction $ Core.FunctionType (nominalApplication tname $ fmap Core.TypeVariable svars) cod)
                        (Substitution.composeTypeSubstList $ (Functions.optionalToList $ fmap Typing.inferenceResultSubst mr) ++ [isubst, subst])

inferTypeOfCollection :: Typing.InferenceContext -> (Core.Type -> Core.Type) -> ([Core.Term] -> Core.Term) -> String -> [Core.Term] -> Compute.Flow s Typing.InferenceResult
inferTypeOfCollection cx typCons trmCons desc els = bindVar withVar
  where
    withVar var = Flows.bind (inferMany cx $ L.zip els $ fmap (\i -> "#" ++ show i) [1..(L.length els)]) fromResults
      where
        fromResults (terms, types, subst1) = mapConstraints cx withConstraints $
            fmap (\t -> Typing.TypeConstraint (Core.TypeVariable var) t desc) types
          where
            withConstraints subst2 = yield iterm itype isubst
              where
                iterm = trmCons terms
                itype = typCons $ Core.TypeVariable var
                isubst = Substitution.composeTypeSubst subst1 subst2

inferTypeOfElimination :: Typing.InferenceContext -> Core.Elimination -> Compute.Flow s Typing.InferenceResult
inferTypeOfElimination cx elm = case elm of
  Core.EliminationProduct tp -> inferTypeOfTupleProjection cx tp
  Core.EliminationRecord p -> inferTypeOfProjection cx p
  Core.EliminationUnion c -> inferTypeOfCaseStatement cx c
  Core.EliminationWrap tname -> inferTypeOfUnwrap cx tname

inferTypeOfFunction :: Typing.InferenceContext -> Core.Function -> Compute.Flow s Typing.InferenceResult
inferTypeOfFunction cx f = case f of
  Core.FunctionElimination elm -> inferTypeOfElimination cx elm
  Core.FunctionLambda l -> inferTypeOfLambda cx l
  Core.FunctionPrimitive name -> inferTypeOfPrimitive cx name

inferTypeOfInjection :: Typing.InferenceContext -> Core.Injection -> Compute.Flow s Typing.InferenceResult
inferTypeOfInjection cx (Core.Injection tname (Core.Field fname term)) = Flows.bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "injected term") withResults
  where
    withResults (Core.TypeScheme svars styp) (Typing.InferenceResult iterm ityp isubst) =
        Flows.bind (ExtractCore.unionType tname styp) withFields
      where
        withFields sfields = Flows.bind (Schemas.findFieldType fname sfields) withField
          where
            withField ftyp = mapConstraints cx withSubst [
                Typing.TypeConstraint ftyp ityp "schema type of injected field"]
              where
                withSubst subst = yield
                  (Terms.inject tname $ Core.Field fname iterm)
                  (nominalApplication tname $ fmap Core.TypeVariable svars)
                  (Substitution.composeTypeSubst isubst subst)

inferTypeOfLambda :: Typing.InferenceContext -> Core.Lambda -> Compute.Flow s Typing.InferenceResult
inferTypeOfLambda cx tmp@(Core.Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = Flows.bind (inferTypeOfTerm cx2 body "lambda body") withResult
      where
        dom = Core.TypeVariable vdom
        cx2 = extendContext [(var, Types.mono dom)] cx
        withResult (Typing.InferenceResult iterm icod isubst) = do
--            checkType vars cx3 rtype rterm
            return $ Typing.InferenceResult rterm rtype isubst
          where
            rterm = Core.TermFunction $ Core.FunctionLambda $ Core.Lambda var (Just rdom) iterm
            rtype = Types.function rdom icod
            rdom = Substitution.substInType isubst dom
            vars = S.union (Rewriting.freeVariablesInType rdom) $ S.union (Rewriting.freeVariablesInType icod) (freeVariablesInContext $ Substitution.substInContext isubst cx2)
            cx3 = Substitution.substInContext isubst cx

-- | Normalize a let term before inferring its type.
inferTypeOfLet :: Typing.InferenceContext -> Core.Let -> Compute.Flow s Typing.InferenceResult
inferTypeOfLet cx (Core.Let bindings0 env0) = Flows.map rewriteResult $ case rewrittenLet of
     Core.TermLet l -> inferTypeOfLetAfterNormalization cx l
     t -> inferTypeOfTerm cx t "empty let term"
  where
    names = fmap Core.letBindingName bindings0
    groups = Sorting.topologicalSortComponents adjList
      where
        adjList = fmap toPair bindings0
        nameSet = S.fromList names
        toPair (Core.LetBinding name term _) = (name, L.filter (\n -> S.member n nameSet) $ S.toList $ Rewriting.freeVariablesInTerm term)
    -- Note: this rewritten let term will yield success in all cases of dependencies among letrec bindings *except*
    --       in cases of polymorphic recursion. In those cases, type hints will be needed (#162).
    rewrittenLet = L.foldl createLet env0 $ L.reverse groups
      where
        bindingMap = M.fromList $ L.zip names bindings0
        createLet e group = Core.TermLet $ Core.Let (Y.catMaybes $ fmap (\n -> M.lookup n bindingMap) group) e
    restoreLet iterm = Core.TermLet $ Core.Let (Y.catMaybes $ fmap (\n -> M.lookup n bindingMap) names) e
      where
        (bindingList, e) = helper (L.length groups) [] iterm
        bindingMap = M.fromList $ fmap (\b -> (Core.letBindingName b, b)) bindingList
        helper level bins term = if level == 0
          then (bins, term)
          else case term of
            Core.TermLet (Core.Let bs e) -> helper (level - 1) (bs ++ bins) e
    rewriteResult result@(Typing.InferenceResult iterm itype isubst) = Typing.InferenceResult (restoreLet iterm) itype isubst

-- | Infer the type of a let (letrec) term which is already in a normal form.
inferTypeOfLetAfterNormalization :: Typing.InferenceContext -> Core.Let -> Compute.Flow s Typing.InferenceResult
inferTypeOfLetAfterNormalization cx0 (Core.Let bins0 env0) = bindVars (L.length bins0) withVars
  where
    bnames = fmap Core.letBindingName bins0
    withVars bvars = Flows.bind (inferTypesOfTemporaryLetBindings cx1 bins0) withInferredBindings
      where
        tbins0 = fmap Core.TypeVariable bvars
        cx1 = extendContext (L.zip bnames $ fmap Types.mono tbins0) cx0
        withInferredBindings (bterms1, tbins1, s1) = Flows.bind
            (Unification.unifyTypeLists (Typing.inferenceContextSchemaTypes cx0) (fmap (Substitution.substInType s1) tbins0) tbins1 "temporary type bindings")
            withSubst
          where
            withSubst s2 = Flows.bind (inferTypeOfTerm (extendContext tsbins1 g2) env0 "let environment") withEnv
              where
                g2 = Substitution.substInContext (Substitution.composeTypeSubst s1 s2) cx0
                tsbins1 = L.zip bnames $ fmap (generalize g2 . Substitution.substInType s2) tbins1
                withEnv (Typing.InferenceResult env1 tenv senv) = do
                    -- TODO: check type
                    return ret
                  where
                    st1 = Typing.TermSubst $ M.fromList $
                      fmap (\(name, ww) -> (name, (Terms.typeApplication (Core.TermVariable name) $ fmap Core.TypeVariable $ Core.typeSchemeVariables ww))) tsbins1
                    -- TODO: should it be composeTypeSubst s2 senv, or even simply s2? We need tests for inferred types in terms.
                    bins1 = fmap (\((name, ts), term) -> Core.LetBinding name
                        (Substitution.substTypesInTerm (Substitution.composeTypeSubst senv s2) $ Terms.typeAbstraction (Core.typeSchemeVariables ts) $ Substitution.substituteInTerm st1 term)
                        (Just $ Substitution.substInTypeScheme senv ts))
                      $ zip tsbins1 bterms1
                    ret = Typing.InferenceResult (Core.TermLet $ Core.Let bins1 env1) tenv (Substitution.composeTypeSubstList [s1, s2, senv])

inferTypeOfList :: Typing.InferenceContext -> [Core.Term] -> Compute.Flow s Typing.InferenceResult
inferTypeOfList cx = inferTypeOfCollection cx Types.list Terms.list "list element"

inferTypeOfLiteral :: Typing.InferenceContext -> Core.Literal -> Compute.Flow s Typing.InferenceResult
inferTypeOfLiteral _ lit = LibFlows.pure $ Typing.InferenceResult (Core.TermLiteral lit) (Core.TypeLiteral $ Variants.literalType lit) Substitution.idTypeSubst

inferTypeOfMap :: Typing.InferenceContext -> M.Map Core.Term Core.Term -> Compute.Flow s Typing.InferenceResult
inferTypeOfMap cx m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        -- TODO: get rid of this special case; it should follow from the generate case
        then LibFlows.pure $ yield (Core.TermMap M.empty) (Types.map (Core.TypeVariable kvar) (Core.TypeVariable vvar)) Substitution.idTypeSubst
        else Flows.bind (inferMany cx $ fmap (\k -> (k, "map key")) $ M.keys m) withKeys
      where
        withKeys (kterms, ktypes, ksubst) = Flows.bind (inferMany cx $ fmap (\v -> (v, "map value")) $ M.elems m) withValues
          where
            withValues (vterms, vtypes, vsubst) = mapConstraints cx withSubst $ kcons ++ vcons
              where
                kcons = fmap (\t -> Typing.TypeConstraint (Core.TypeVariable kvar) t "map key") ktypes
                vcons = fmap (\t -> Typing.TypeConstraint (Core.TypeVariable vvar) t "map value") vtypes
                withSubst subst = yield
                    (Core.TermMap $ M.fromList $ L.zip kterms vterms)
                    (Types.map (Core.TypeVariable kvar) (Core.TypeVariable vvar))
                    (Substitution.composeTypeSubstList [ksubst, vsubst, subst])

inferTypeOfOptional :: Typing.InferenceContext -> Maybe Core.Term -> Compute.Flow s Typing.InferenceResult
inferTypeOfOptional cx m = inferTypeOfCollection cx Types.optional trmCons "optional element" $ Y.maybe [] (\e -> [e]) m
  where
    trmCons terms = case terms of
      [] -> Terms.optional Nothing
      [term] -> Terms.optional $ Just term

inferTypeOfPrimitive :: Typing.InferenceContext -> Core.Name -> Compute.Flow s Typing.InferenceResult
inferTypeOfPrimitive cx name = case M.lookup name (Typing.inferenceContextPrimitiveTypes cx) of
    Nothing -> LibFlows.fail $ "No such primitive: " ++ Core.unName name
    Just scheme -> Flows.bind (instantiateTypeScheme scheme) withScheme
  where
    withScheme ts = yieldChecked cx (Core.typeSchemeVariables ts) iterm itype Substitution.idTypeSubst
      where
        iterm = L.foldl (\t v -> Core.TermTypeApplication $ Core.TypedTerm t $ Core.TypeVariable v) (Terms.primitive name) $ Core.typeSchemeVariables ts
        itype = Core.typeSchemeType ts

inferTypeOfProduct :: Typing.InferenceContext -> [Core.Term] -> Compute.Flow s Typing.InferenceResult
inferTypeOfProduct cx els = Flows.map withResults (inferMany cx $ fmap (\e -> (e, "tuple element")) els)
  where
    withResults (iterms, itypes, isubst) = yield (Terms.tuple iterms) (Types.product itypes) isubst

inferTypeOfProjection :: Typing.InferenceContext -> Core.Projection -> Compute.Flow s Typing.InferenceResult
inferTypeOfProjection cx (Core.Projection tname fname) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (Core.TypeScheme svars styp) = Flows.bind (ExtractCore.recordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ Schemas.findFieldType fname sfields
          where
            withField ftyp = yield
              (Terms.project tname fname)
              (Types.function (nominalApplication tname $ fmap Core.TypeVariable svars) ftyp)
              Substitution.idTypeSubst

inferTypeOfRecord :: Typing.InferenceContext -> Core.Record -> Compute.Flow s Typing.InferenceResult
inferTypeOfRecord cx (Core.Record tname fields) =
    Flows.bind2 (requireSchemaType cx tname) (inferMany cx $ fmap (\f -> (Core.fieldTerm f, "field " ++ Core.unName (Core.fieldName f))) fields) withResults
  where
    fnames = fmap Core.fieldName fields
    withResults (Core.TypeScheme svars styp) (iterms, itypes, isubst) = mapConstraints cx withSubst [
        Typing.TypeConstraint styp ityp "schema type of record"]
      where
        ityp = Core.TypeRecord $ Core.RowType tname $ L.zipWith Core.FieldType fnames itypes
        withSubst subst = yield
            (Core.TermRecord $ Core.Record tname $ L.zipWith Core.Field fnames iterms)
            (nominalApplication tname $ fmap Core.TypeVariable svars)
            (Substitution.composeTypeSubst isubst subst)

inferTypeOfSet :: Typing.InferenceContext -> S.Set Core.Term -> Compute.Flow s Typing.InferenceResult
inferTypeOfSet cx = inferTypeOfCollection cx Types.set (Terms.set . S.fromList) "set element" . S.toList

inferTypeOfSum :: Typing.InferenceContext -> Core.Sum -> Compute.Flow s Typing.InferenceResult
inferTypeOfSum cx (Core.Sum i s term) = bindInferredTerm cx term "sum term" withResult
  where
    withResult (Typing.InferenceResult iterm ityp isubst) = Flows.map withVars (LibFlows.sequence $ fmap (varOrTerm ityp) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then LibFlows.pure $ Left t
          else Right <$> freshName
        withVars vars = yield (Core.TermSum $ Core.Sum i s iterm) (Core.TypeSum $ fmap toType vars) isubst
          where
            toType e = case e of
              Left t -> t
              Right v -> Core.TypeVariable v

inferTypeOfTerm :: Typing.InferenceContext -> Core.Term -> String -> Compute.Flow s Typing.InferenceResult
inferTypeOfTerm cx term desc = Flows.withTrace desc $ case term of
  Core.TermAnnotated a -> inferTypeOfAnnotatedTerm cx a
  Core.TermApplication a -> inferTypeOfApplication cx a
  Core.TermFunction f -> inferTypeOfFunction cx f
  Core.TermLet l -> inferTypeOfLet cx l
  Core.TermList els -> inferTypeOfList cx els
  Core.TermLiteral l -> inferTypeOfLiteral cx l
  Core.TermMap m -> inferTypeOfMap cx m
  Core.TermOptional m -> inferTypeOfOptional cx m
  Core.TermProduct els -> inferTypeOfProduct cx els
  Core.TermRecord r -> inferTypeOfRecord cx r
  Core.TermSet s -> inferTypeOfSet cx s
  Core.TermSum s -> inferTypeOfSum cx s
  Core.TermTypeAbstraction ta -> inferTypeOfTypeAbstraction cx ta
  Core.TermTypeApplication tt -> inferTypeOfTypeApplication cx tt
  Core.TermTyped t -> inferTypeOfTypedTerm cx t
  Core.TermUnion i -> inferTypeOfInjection cx i
  Core.TermVariable name -> inferTypeOfVariable cx name
  Core.TermWrap w -> inferTypeOfWrappedTerm cx w

inferTypeOfTupleProjection :: Typing.InferenceContext -> Core.TupleProjection -> Compute.Flow s Typing.InferenceResult
inferTypeOfTupleProjection _ (Core.TupleProjection arity idx _) = forVars arity withVars
  where
    withVars vars = yield
        (Terms.untuple arity idx $ Just types)
        (Types.function (Types.product types) cod)
        Substitution.idTypeSubst
      where
        types = Core.TypeVariable <$> vars
        cod = types !! idx

inferTypeOfTypeAbstraction :: Typing.InferenceContext -> Core.TypeAbstraction -> Compute.Flow s Typing.InferenceResult
inferTypeOfTypeAbstraction cx (Core.TypeAbstraction _ term) = inferTypeOfTerm cx term "type abstraction"

inferTypeOfTypeApplication :: Typing.InferenceContext -> Core.TypedTerm -> Compute.Flow s Typing.InferenceResult
inferTypeOfTypeApplication cx (Core.TypedTerm term _) = inferTypeOfTerm cx term "type application term"

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: Typing.InferenceContext -> Core.TypedTerm -> Compute.Flow s Typing.InferenceResult
inferTypeOfTypedTerm cx (Core.TypedTerm term _) = inferTypeOfTerm cx term "typed term"

inferTypeOfUnwrap :: Typing.InferenceContext -> Core.Name -> Compute.Flow s Typing.InferenceResult
inferTypeOfUnwrap cx tname = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (Core.TypeScheme svars styp) = Flows.map withWrappedType (ExtractCore.wrappedType tname styp)
      where
        withWrappedType wtyp = yield
          (Terms.unwrap tname)
          (Types.function (nominalApplication tname $ fmap Core.TypeVariable svars) wtyp)
          Substitution.idTypeSubst

inferTypeOfVariable :: Typing.InferenceContext -> Core.Name -> Compute.Flow s Typing.InferenceResult
inferTypeOfVariable cx name = case M.lookup name (Typing.inferenceContextDataTypes cx) of
    Nothing -> LibFlows.fail $ "Variable not bound to type: " ++ Core.unName name
    Just scheme -> Flows.bind (instantiateTypeScheme scheme) withTypeScheme
  where
    withTypeScheme (Core.TypeScheme vars itype) = do
--        checkType (S.fromList vars) cx itype iterm
        return $ Typing.InferenceResult iterm itype Substitution.idTypeSubst
      where
        iterm = Terms.typeApplication (Core.TermVariable name) $ fmap Core.TypeVariable vars

inferTypeOfWrappedTerm :: Typing.InferenceContext -> Core.WrappedTerm -> Compute.Flow s Typing.InferenceResult
inferTypeOfWrappedTerm cx (Core.WrappedTerm tname term) =
  Flows.bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "wrapped term") withResult
  where
    withResult (Core.TypeScheme svars styp) (Typing.InferenceResult iterm ityp isubst) = do
      freshVars <- freshNames (length svars)
      let subst = Typing.TypeSubst (M.fromList (zip svars (fmap Core.TypeVariable freshVars)))
          stypInst = Substitution.substInType subst styp
          nominalInst = nominalApplication tname $ fmap Core.TypeVariable freshVars
          expected = Core.TypeWrap (Core.WrappedType tname ityp)
          freeVars = S.toList $
            Rewriting.freeVariablesInType ityp `S.union`
            Rewriting.freeVariablesInTerm iterm `S.union`
            S.fromList freshVars

      bindConstraints cx
        (\subst2 ->
           yieldChecked cx freeVars
             (Terms.wrap tname iterm)
             nominalInst
             (Substitution.composeTypeSubst isubst subst2))
        [Typing.TypeConstraint stypInst expected "schema type of wrapper"]

inferTypesOfTemporaryLetBindings :: Typing.InferenceContext -> [Core.LetBinding] -> Compute.Flow s ([Core.Term], [Core.Type], Typing.TypeSubst)
inferTypesOfTemporaryLetBindings cx bins = case bins of
  [] -> return ([], [], Substitution.idTypeSubst)
  ((Core.LetBinding k v _):tl) -> do
    Typing.InferenceResult j u' u <- inferTypeOfTerm cx v $ "temporary let binding '" ++ Core.unName k ++ "'"
    (h, r', r) <- inferTypesOfTemporaryLetBindings (Substitution.substInContext u cx) tl
    let ret = ((Substitution.substTypesInTerm r j):h, (Substitution.substInType r u'):r', Substitution.composeTypeSubst u r)
    --check0 (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
    return ret

--------------------------------------------------------------------------------

bindConstraints :: Typing.InferenceContext -> (Typing.TypeSubst -> Compute.Flow s a) -> [Typing.TypeConstraint] -> Compute.Flow s a
bindConstraints cx f constraints = Flows.bind (Unification.unifyTypeConstraints (Typing.inferenceContextSchemaTypes cx) constraints) f

bindVar :: (Core.Name -> Compute.Flow s a) -> Compute.Flow s a
bindVar = Flows.bind freshName

bindVar2 :: (Core.Name -> Core.Name -> Compute.Flow s a) -> Compute.Flow s a
bindVar2 f = bindVar $ \v1 -> bindVar $ \v2 -> f v1 v2

bindVars :: Int -> ([Core.Name] -> Compute.Flow s a) -> Compute.Flow s a
bindVars n = Flows.bind $ freshNames n

bindInferredTerm :: Typing.InferenceContext -> Core.Term -> String -> (Typing.InferenceResult -> Compute.Flow s a) -> Compute.Flow s a
bindInferredTerm cx term desc = Flows.bind $ inferTypeOfTerm cx term desc

forInferredTerm :: Typing.InferenceContext -> Core.Term -> String -> (Typing.InferenceResult -> a) -> Compute.Flow s a
forInferredTerm cx term desc f = Flows.map f $ inferTypeOfTerm cx term desc

forVar :: (Core.Name -> a) -> Compute.Flow s a
forVar f = Flows.map f freshName

forVars :: Int -> ([Core.Name] -> a) -> Compute.Flow s a
forVars n f = Flows.map f $ freshNames n

fTypeToTypeScheme :: Core.Type -> Core.TypeScheme
fTypeToTypeScheme typ = gather [] typ
  where
    gather vars typ = case Strip.stripType typ of
      Core.TypeForall (Core.ForallType v body) -> gather (v:vars) body
      t -> Core.TypeScheme (L.reverse vars) t

instantiateFType :: Core.Type -> Compute.Flow s Core.Type
instantiateFType typ = do
  Core.TypeScheme _ t <- instantiateTypeScheme (fTypeToTypeScheme typ)
  return t

instantiateTypeScheme :: Core.TypeScheme -> Compute.Flow s Core.TypeScheme
instantiateTypeScheme scheme = Flows.map doSubst (freshNames $ L.length oldVars)
  where
    doSubst newVars = Core.TypeScheme newVars $ Substitution.substInType subst $ Core.typeSchemeType scheme
      where
        subst = Typing.TypeSubst $ M.fromList $ L.zip oldVars (Core.TypeVariable <$> newVars)
    oldVars = Core.typeSchemeVariables scheme

mapConstraints :: Typing.InferenceContext -> (Typing.TypeSubst -> a) -> [Typing.TypeConstraint] -> Compute.Flow s a
mapConstraints cx f constraints = Flows.map f $ Unification.unifyTypeConstraints (Typing.inferenceContextSchemaTypes cx) constraints

nominalApplication :: Core.Name -> [Core.Type] -> Core.Type
nominalApplication tname args = L.foldl (\t a -> Types.apply t a) (Core.TypeVariable tname) args

requireSchemaType :: Typing.InferenceContext -> Core.Name -> Compute.Flow s Core.TypeScheme
requireSchemaType cx tname = case M.lookup tname (Typing.inferenceContextSchemaTypes cx) of
  Nothing -> LibFlows.fail $ "No such schema type: " ++ Core.unName tname
  Just ts -> instantiateTypeScheme $ Rewriting.stripTypeSchemeRecursive ts

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: [(Core.Name, Core.TypeScheme)] -> Typing.InferenceContext -> Typing.InferenceContext
extendContext pairs cx = cx {Typing.inferenceContextDataTypes = M.union (M.fromList pairs) (Typing.inferenceContextDataTypes cx)}

yield :: Core.Term -> Core.Type -> Typing.TypeSubst -> Typing.InferenceResult
yield term typ subst = Typing.InferenceResult (Substitution.substTypesInTerm subst term) (Substitution.substInType subst typ) subst

yieldChecked :: Typing.InferenceContext -> [Core.Name] -> Core.Term -> Core.Type -> Typing.TypeSubst -> Compute.Flow s Typing.InferenceResult
yieldChecked cx vars term typ subst = do
--    checkType (S.fromList vars) cx itype iterm
    return $ Typing.InferenceResult iterm itype subst
  where
    iterm = Substitution.substTypesInTerm subst term
    itype = Substitution.substInType subst typ

yieldDebug :: Typing.InferenceContext -> String -> Core.Term -> Core.Type -> Typing.TypeSubst -> Compute.Flow s Typing.InferenceResult
yieldDebug cx debugId term typ subst = do

    Annotations.debugIf debugId $ ""
        ++ "\n\tterm: " ++ ShowCore.term term
        ++ "\n\ttyp: " ++ ShowCore.type_ typ
        ++ "\n\tsubst: " ++ ShowCore.typeSubst subst
        ++ "\n\trterm: " ++ ShowCore.term rterm
        ++ "\n\trtyp: " ++ ShowCore.type_ rtyp

    return $ Typing.InferenceResult rterm rtyp subst
  where
    rterm = Substitution.substTypesInTerm subst term
    rtyp = Substitution.substInType subst typ