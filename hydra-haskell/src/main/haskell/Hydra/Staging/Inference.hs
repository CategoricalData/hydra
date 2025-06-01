module Hydra.Staging.Inference where

import Hydra.Coders
import Hydra.Compute
import Hydra.Constants
import Hydra.Core
import Hydra.Errors
import Hydra.Flows
import Hydra.Functions
import Hydra.Graph
import Hydra.Typing
import Hydra.Lib.Flows as Flows
import Hydra.Lib.Io
import Hydra.Mantle
import Hydra.Rewriting
import Hydra.Inference
import Hydra.Staging.Annotations
import Hydra.Staging.CoreDecoding
import Hydra.Staging.Lexical
import Hydra.Staging.Rewriting
import Hydra.Staging.Schemas
import Hydra.Staging.Sorting
import Hydra.Strip
import Hydra.Substitution
import Hydra.Unification
import Hydra.Variants
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Expect as Expect

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

key_vcount = Name "inferenceTypeVariableCount"

freshName :: Flow s Name
freshName = normalTypeVariable <$> nextCount key_vcount

freshNames :: Int -> Flow s [Name]
freshNames n = Flows.sequence $ L.replicate n freshName

freshVariableType :: Flow s Type
freshVariableType = TypeVariable <$> freshName

--------------------------------------------------------------------------------
-- Type checking

-- | A local typing context for inference, mapping term variables to types.
type Types = M.Map Name Type

typeOf :: InferenceContext -> S.Set Name -> Types -> Term -> Flow s Type
--typeOf cx vars types term = case term of
typeOf cx vars types term = withTrace ("checking type of: " ++ showTerm term ++ " (vars: " ++ show (fmap unName $ S.toList vars) ++ ", types: " ++ (show types) ++ ")") $ case term of
    TermAnnotated (AnnotatedTerm term1 _) -> typeOf cx vars types term1
    TermApplication (Application a b) -> do
      t1 <- typeOf cx vars types a
      t2 <- typeOf cx vars types b
      checkTypeVariables vars t1
      checkTypeVariables vars t2
      case t1 of
        TypeFunction (FunctionType p q) -> if p == t2
          then return q
          else Flows.fail $ "expected " ++ showType p ++ " in " ++ showTerm term ++ " but found " ++ showType t2
        _ -> Flows.fail $ "left hand side of application " ++ showTerm term ++ " is not a function type: " ++ showType t1
    TermFunction f -> case f of
      FunctionElimination elm -> case elm of
        EliminationProduct (TupleProjection index arity mtypes) -> case mtypes of
          Nothing -> Flows.fail $ "untyped tuple projection: " ++ showTerm term
          Just types -> do
            CM.mapM (checkTypeVariables vars) types
            return $ Types.function (Types.product types) (types !! index)
--        EliminationRecord (Projection tname (Field fname fterm)) -> ...
--        EliminationUnion (CaseStatement tname def cases) -> ...
--        EliminationWrap tname -> ...
      FunctionLambda (Lambda x mt e) -> case mt of
        Nothing -> Flows.fail $ "untyped lambda: " ++ showTerm term
        Just t -> do
          checkTypeVariables vars t
          t1 <- typeOf cx vars (M.insert x t types) e
          checkTypeVariables vars t1
          return $ Types.function t t1
      FunctionPrimitive name -> typeSchemeToFType <$> ts
        where
          -- Note: no instantiation
          ts = case M.lookup name (inferenceContextPrimitiveTypes cx) of
            Nothing -> Flows.fail $ "no such primitive: " ++ unName name
            Just ts0 -> return ts0
    TermLet (Let es e) -> do
        btypes <- CM.mapM binType es
        let types2 = M.union (M.fromList $ L.zip bnames btypes) types
        est <- CM.mapM (\v -> typeOf cx vars types2 v) bterms
        CM.mapM (checkTypeVariables vars) est
        CM.mapM (checkTypeVariables vars) btypes
        if est == btypes
          then typeOf cx vars types2 e
          else Flows.fail $ "binding types disagree: " ++ show (fmap showType est) ++ " and " ++ show (fmap showType btypes)
      where
        bnames = fmap letBindingName es
        bterms = fmap letBindingTerm es
        binType b = case letBindingType b of
          Nothing -> Flows.fail $ "untyped let binding in " ++ showTerm term
          Just ts -> return $ typeSchemeToFType ts
    TermList els -> case els of
      [] -> do
        t <- freshName
        let var = TypeVariable t
        return $ TypeForall $ ForallType t (TypeList var)
      (x:xs) -> do
        tx <- typeOf cx vars types x >>= instantiateFType -- TODO: is instantiation really necessary?
        CM.forM_ xs $ \e -> do
          t <- typeOf cx vars types e >>= instantiateFType -- TODO: is instantiation really necessary?
          unifyTypes (inferenceContextSchemaTypes cx) t tx "type check over collection"
        checkTypeVariables vars tx
        return $ TypeList tx
    TermLiteral lit -> return $ TypeLiteral $ literalType lit
    TermMap m -> if M.null m
        then return $ typeSchemeToFType $ Types.poly ["k", "v"] $ Types.map (Types.var "k") (Types.var "v")
        else do
          kt <- (CM.mapM (typeOf cx vars types) $ fmap fst pairs) >>= singleType "map keys"
          vt <- (CM.mapM (typeOf cx vars types) $ fmap snd pairs) >>= singleType "map values"
          checkTypeVariables vars kt
          checkTypeVariables vars vt
          return $ TypeMap $ MapType kt vt
      where
        pairs = M.toList m
    TermOptional mt -> typeOfCollection cx "optional" TypeOptional vars types $ Y.maybe [] (\x -> [x]) mt
    TermProduct tuple -> do
      etypes <- CM.mapM (typeOf cx vars types) tuple
      CM.mapM (checkTypeVariables vars) etypes
      return $ TypeProduct etypes

    TermRecord (Record tname fields) -> do
      ftypes <- CM.mapM (typeOf cx vars types) $ fmap fieldTerm fields
      CM.mapM (checkTypeVariables vars) ftypes
      typeOfNominal "record typeOf" cx tname $ TypeRecord $ RowType tname $ L.zipWith FieldType (fmap fieldName fields) ftypes

    TermSet els -> typeOfCollection cx "set" TypeSet vars types $ S.toList els
--    TermSum (Sum idx size term1) -> ...
    TermTypeAbstraction (TypeAbstraction v e) -> do
      t1 <- typeOf cx (S.insert v vars) types e
      checkTypeVariables (S.insert v vars) t1
      return $ TypeForall $ ForallType v t1
    TermTypeApplication (TypedTerm e t) -> do
      t1 <- typeOf cx vars types e
      checkTypeVariables vars t1
--      Flows.fail $ "type-checking type application"
--        ++ "\n\tterm: " ++ showTerm e
--        ++ "\n\ttype: " ++ showType t
--        ++ "\n\tvars: " ++ show (fmap unName $ S.toList vars)
--        ++ "\n\tt1: " ++ showType t1
      case t1 of
        TypeForall (ForallType v t2) -> return $ substInType (TypeSubst $ M.fromList [(v, t)]) t2
        _ -> Flows.fail $ "not a forall type: " ++ showType t1 ++ " in " ++ showTerm term
    TermUnion (Injection tname (Field fname term1)) -> do
        ftype <- typeOf cx vars types term1
        checkTypeVariables vars ftype

        TypeScheme svars styp <- requireSchemaType cx tname
        sfields <- Expect.unionType tname styp
        let fnames = fmap fieldTypeName sfields
        ftypes <- CM.mapM (fieldTypeOf ftype) fnames

        let expected = TypeUnion $ RowType tname $ L.zipWith FieldType fnames ftypes
        (TypeSubst subst) <- unifyTypes (inferenceContextSchemaTypes cx) styp expected "union typeOf"

        let tparams = fmap (resolveType subst) svars
        return $ nominalApplication tname tparams
      where
        fieldTypeOf ftype fname1 = if fname1 == fname
          then return ftype
          else TypeVariable <$> freshName
        resolveType subst v = Y.fromMaybe (TypeVariable v) $ M.lookup v subst

    TermVariable name -> case M.lookup name types of
      Nothing -> Flows.fail $ "unbound variable: " ++ unName name
      Just t -> return t

    TermWrap (WrappedTerm tname innerTerm) -> do
      innerType <- typeOf cx vars types innerTerm
      checkTypeVariables vars innerType
      typeOfNominal "wrapper typeOf" cx tname $ TypeWrap $ WrappedType tname innerType

    _ -> Flows.fail $ "unsupported term variant in typeOf: " ++ show (termVariant term)

typeOfCollection :: InferenceContext -> String -> (Type -> Type) -> S.Set Name -> Types -> [Term] -> Flow s Type
typeOfCollection cx desc cons vars types els = if L.null els
  then return $ typeSchemeToFType $ Types.poly ["t"] $ cons $ Types.var "t"
  else do
    et <- CM.mapM (typeOf cx vars types) els >>= singleType desc
    checkTypeVariables vars et
    return $ cons et

typeOfNominal :: String -> InferenceContext -> Name -> Type -> Flow s Type
typeOfNominal desc cx tname expected = do
    TypeScheme svars styp <- requireSchemaType cx tname

--      Flows.fail $ "svars: " ++ show svars ++ ", styp: " ++ showType styp
--      Flows.fail $ "expected: " ++ showType expected ++ ", schema type: " ++ showType styp

    (TypeSubst subst) <- unifyTypes (inferenceContextSchemaTypes cx) styp expected desc

    let tparams = fmap (resolveType subst) svars
    return $ nominalApplication tname tparams
  where
    resolveType subst v = Y.fromMaybe (TypeVariable v) $ M.lookup v subst

singleType :: String -> [Type] -> Flow s Type
singleType desc types = if (L.foldl (\b t -> b && t == h) True types)
    then return h
    else Flows.fail $ "unequal types " ++ show (fmap showType types) ++ " in " ++ desc
  where
    h = L.head types

checkType :: S.Set Name -> InferenceContext -> Type -> Term -> Flow s ()
checkType k g t e = if debugInference
  then do
    t0 <- typeOf g k (toFContext g) e
    if t0 == t
      then return ()
      else Flows.fail $ "type checking failed: expected " ++ showType t ++ " but found " ++ showType t0
  else return ()

checkTypeVariables :: S.Set Name -> Type -> Flow s ()
checkTypeVariables vars typ = case typ of
  TypeForall (ForallType v body) -> checkTypeVariables (S.insert v vars) body
  TypeVariable v -> if S.member v vars
    then return ()
    else Flows.fail $ "unbound type variable \"" ++ unName v ++ "\" in " ++ showType typ
  _ -> do
    CM.sequence $ fmap (checkTypeVariables vars) $ subtypes typ
    return ()

typeSchemeToFType :: TypeScheme -> Type
typeSchemeToFType (TypeScheme vars body) = L.foldl (\t v -> TypeForall $ ForallType v t) body $ L.reverse vars

toFContext :: InferenceContext -> Types
toFContext = fmap typeSchemeToFType . inferenceContextDataTypes

--------------------------------------------------------------------------------
-- Inference

showInferenceResult :: InferenceResult -> String
showInferenceResult (InferenceResult term typ subst) = "{"
    ++ "term=" ++ showTerm term ++ ", "
    ++ "type= " ++ showType typ ++ ", "
    ++ "subst= " ++ showTypeSubst subst ++ "}"

freeVariablesInContext :: InferenceContext -> S.Set Name
freeVariablesInContext cx = L.foldl S.union S.empty $ fmap freeVariablesInTypeSchemeSimple $ M.elems $ inferenceContextDataTypes cx

generalize :: InferenceContext -> Type -> TypeScheme
generalize cx typ = TypeScheme vars typ
  where
    vars = L.nub $ L.filter isUnbound $ S.toList $ freeVariablesInType typ
    isUnbound v = not (S.member v $ freeVariablesInContext cx)
      && not (M.member v $ inferenceContextSchemaTypes cx)

graphToInferenceContext :: Graph -> Flow s InferenceContext
graphToInferenceContext g0 = do
    schemaTypes <- schemaGraphToTypingEnvironment schema
    return $ InferenceContext schemaTypes primTypes varTypes False
  where
    schema = Y.fromMaybe g0 $ graphSchema g0
    primTypes = M.fromList $ fmap (\p -> (primitiveName p, primitiveType p)) (M.elems $ graphPrimitives g0)
    varTypes = M.empty

-- Note: this operation is expensive, as it creates a new typing environment for each individual term
inferInGraphContext :: Term -> Flow Graph InferenceResult
inferInGraphContext term = do
  g <- getState
  cx <- graphToInferenceContext g
  inferTypeOfTerm cx term "single term"

inferGraphTypes :: Graph -> Flow s Graph
inferGraphTypes g0 = withTrace "graph inference" $ do
    cx <- graphToInferenceContext g0
    Flows.bind (inferTypeOfTerm cx (toLetTerm g0) "graph term") withResult
  where
    toLetTerm g = TermLet $ Let (fmap toBinding $ M.elems $ graphElements g) $ graphBody g
      where
        toBinding (Element name term _) = LetBinding name term Nothing
    withResult (InferenceResult term ts _) = case normalizeTypeVariablesInTerm term of
      TermLet l -> Flows.pure $ fromLetTerm l
      _ -> Flows.fail $ "Expected inferred graph as let term"
    fromLetTerm (Let bindings env) = g0 {
        graphElements = M.fromList $ fmap fromBinding bindings,
        graphBody = env,
        graphEnvironment = M.empty,
        graphTypes = M.empty}
      where
        fromBinding (LetBinding name term mt) = (name, Element name term mt)

inferMany :: InferenceContext -> [(Term, String)] -> Flow s ([Term], [Type], TypeSubst)
inferMany cx pairs = case pairs of
  [] -> return ([], [], idTypeSubst)
  ((e, desc):tl) -> do
    (InferenceResult e1 t1 s1) <- inferTypeOfTerm cx e desc
    (e2, t2, s2) <- inferMany (substInContext s1 cx) tl
    return (
      (substTypesInTerm s2 e1):e2,
      (substInType s2 t1):t2,
      composeTypeSubst s1 s2)

inferTwo :: InferenceContext -> Term -> String -> Term -> String -> Flow s (Term, Type, Term, Type, TypeSubst)
inferTwo cx term1 desc1 term2 desc2 = Flows.map withResult $ inferMany cx [(term1, desc1), (term2, desc2)]
  where
    withResult ([e1, e2], [t1, t2], s) = (e1, t1, e2, t2, s)
    withResult _ = error "unexpected result from inferMany"

inferTypeOf :: InferenceContext -> Term -> Flow s (Term, TypeScheme)
inferTypeOf cx term = bindInferredTerm cx letTerm "infer type of term" unifyAndSubst
  where
    letTerm = TermLet $ Let [LetBinding (Name "ignoredVariableName") term Nothing] $ Terms.string "ignoredEnvironment"
    unifyAndSubst result = do
        (Let bindings _) <- withEmptyGraph $ Expect.letTerm $ normalizeTypeVariablesInTerm $ inferenceResultTerm result
        case bindings of
          [LetBinding _ term1 (Just ts)] -> return (term1, ts)
          _ -> Flows.fail $ "Expected a single binding with a type scheme, but got: " ++ show bindings
      where
        subst = inferenceResultSubst result

inferTypeOfAnnotatedTerm :: InferenceContext -> AnnotatedTerm -> Flow s InferenceResult
inferTypeOfAnnotatedTerm cx (AnnotatedTerm term ann) = Flows.map withResult $ inferTypeOfTerm cx term "annotated term"
  where
    withResult (InferenceResult iterm itype isubst)
      = InferenceResult (TermAnnotated $ AnnotatedTerm iterm ann) itype isubst

inferTypeOfApplication :: InferenceContext -> Application -> Flow s InferenceResult
inferTypeOfApplication cx (Application e0 e1) = bindInferredTerm cx e0 "lhs" withLhs
  where
    withLhs (InferenceResult a t0 s0) = bindInferredTerm (substInContext s0 cx) e1 "rhs" withRhs
      where
        withRhs (InferenceResult b t1 s1) = bindVar withVar
          where
            withVar v = Flows.map withSubst $ unifyTypes
                (inferenceContextSchemaTypes cx)
                (substInType s1 t0)
                (Types.function t1 $ TypeVariable v)
                "application lhs"
              where
                withSubst s2 = InferenceResult rExpr rType rSubst
                  where
                    rExpr = Terms.apply (substTypesInTerm (composeTypeSubst s1 s2) a) (substTypesInTerm s2 b)
                    rType = substInType s2 $ TypeVariable v
                    rSubst = composeTypeSubstList [s0, s1, s2]

inferTypeOfCaseStatement :: InferenceContext -> CaseStatement -> Flow s InferenceResult
inferTypeOfCaseStatement cx (CaseStatement tname dflt cases) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    fnames = fmap fieldName cases
    withSchemaType (TypeScheme svars styp) = Flows.bind (Expect.unionType tname styp) withFields
      where
        withFields sfields = bind2 (traverse (\t -> inferTypeOfTerm cx t "default") dflt) (inferMany cx $ fmap (\f -> (fieldTerm f, "case " ++ unName (fieldName f))) cases) withResults
          where
            withResults mr (iterms, itypes, isubst) = bindVar withCod
              where
                withCod codv = mapConstraints cx withConstraints (dfltConstraints ++ caseConstraints)
                  where
                    cod = TypeVariable codv
                    dfltConstraints = optionalToList $ fmap (\r -> TypeConstraint cod (inferenceResultType r) "match default") mr
                    caseConstraints = Y.catMaybes $ L.zipWith (\fname itype -> fmap (\r -> toConstraint r itype) $ M.lookup fname caseMap) fnames itypes
                      where
                        caseMap = M.fromList $ fmap (\(FieldType fname ftype) -> (fname, ftype)) sfields
                        toConstraint ftyp r = TypeConstraint r (Types.function ftyp cod) "case type"
                    withConstraints subst = yield
                        (TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement tname (fmap inferenceResultTerm mr) $ L.zipWith Field fnames iterms)
                        (TypeFunction $ FunctionType (nominalApplication tname $ fmap TypeVariable svars) cod)
                        (composeTypeSubstList $ (optionalToList $ fmap inferenceResultSubst mr) ++ [isubst, subst])

inferTypeOfCollection :: InferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s InferenceResult
inferTypeOfCollection cx typCons trmCons desc els = bindVar withVar
  where
    withVar var = Flows.bind (inferMany cx $ L.zip els $ fmap (\i -> "#" ++ show i) [1..(L.length els)]) fromResults
      where
        fromResults (terms, types, subst1) = mapConstraints cx withConstraints $
            fmap (\t -> TypeConstraint (TypeVariable var) t desc) types
          where
            withConstraints subst2 = yield iterm itype isubst
              where
                iterm = trmCons terms
                itype = typCons $ TypeVariable var
                isubst = composeTypeSubst subst1 subst2

inferTypeOfElimination :: InferenceContext -> Elimination -> Flow s InferenceResult
inferTypeOfElimination cx elm = case elm of
  EliminationProduct tp -> inferTypeOfTupleProjection cx tp
  EliminationRecord p -> inferTypeOfProjection cx p
  EliminationUnion c -> inferTypeOfCaseStatement cx c
  EliminationWrap tname -> inferTypeOfUnwrap cx tname

inferTypeOfFunction :: InferenceContext -> Function -> Flow s InferenceResult
inferTypeOfFunction cx f = case f of
  FunctionElimination elm -> inferTypeOfElimination cx elm
  FunctionLambda l -> inferTypeOfLambda cx l
  FunctionPrimitive name -> inferTypeOfPrimitive cx name

inferTypeOfInjection :: InferenceContext -> Injection -> Flow s InferenceResult
inferTypeOfInjection cx (Injection tname (Field fname term)) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "injected term") withResults
  where
    withResults (TypeScheme svars styp) (InferenceResult iterm ityp isubst) =
        Flows.bind (Expect.unionType tname styp) withFields
      where
        withFields sfields = Flows.bind (findFieldType fname sfields) withField
          where
            withField ftyp = mapConstraints cx withSubst [
                TypeConstraint ftyp ityp "schema type of injected field"]
              where
                withSubst subst = yield
                  (Terms.inject tname $ Field fname iterm)
                  (nominalApplication tname $ fmap TypeVariable svars)
                  (composeTypeSubst isubst subst)

inferTypeOfLambda :: InferenceContext -> Lambda -> Flow s InferenceResult
inferTypeOfLambda cx tmp@(Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = Flows.bind (inferTypeOfTerm cx2 body "lambda body") withResult
      where
        dom = TypeVariable vdom
        cx2 = extendContext [(var, Types.mono dom)] cx
        withResult (InferenceResult iterm icod isubst) = do
--            checkType vars cx3 rtype rterm
            return $ InferenceResult rterm rtype isubst
          where
            rterm = TermFunction $ FunctionLambda $ Lambda var (Just rdom) iterm
            rtype = Types.function rdom icod
            rdom = substInType isubst dom
            vars = S.union (freeVariablesInType rdom) $ S.union (freeVariablesInType icod) (freeVariablesInContext $ substInContext isubst cx2)
            cx3 = substInContext isubst cx

-- | Normalize a let term before inferring its type.
inferTypeOfLet :: InferenceContext -> Let -> Flow s InferenceResult
inferTypeOfLet cx (Let bindings0 env0) = Flows.map rewriteResult $ case rewrittenLet of
     TermLet l -> inferTypeOfLetAfterNormalization cx l
     t -> inferTypeOfTerm cx t "empty let term"
  where
    names = fmap letBindingName bindings0
    groups = topologicalSortComponents adjList
      where
        adjList = fmap toPair bindings0
        nameSet = S.fromList names
        toPair (LetBinding name term _) = (name, L.filter (\n -> S.member n nameSet) $ S.toList $ freeVariablesInTerm term)
    -- Note: this rewritten let term will yield success in all cases of dependencies among letrec bindings *except*
    --       in cases of polymorphic recursion. In those cases, type hints will be needed (#162).
    rewrittenLet = L.foldl createLet env0 $ L.reverse groups
      where
        bindingMap = M.fromList $ L.zip names bindings0
        createLet e group = TermLet $ Let (Y.catMaybes $ fmap (\n -> M.lookup n bindingMap) group) e
    restoreLet iterm = TermLet $ Let (Y.catMaybes $ fmap (\n -> M.lookup n bindingMap) names) e
      where
        (bindingList, e) = helper (L.length groups) [] iterm
        bindingMap = M.fromList $ fmap (\b -> (letBindingName b, b)) bindingList
        helper level bins term = if level == 0
          then (bins, term)
          else case term of
            TermLet (Let bs e) -> helper (level - 1) (bs ++ bins) e
    rewriteResult result@(InferenceResult iterm itype isubst) = InferenceResult (restoreLet iterm) itype isubst

-- | Infer the type of a let (letrec) term which is already in a normal form.
inferTypeOfLetAfterNormalization :: InferenceContext -> Let -> Flow s InferenceResult
inferTypeOfLetAfterNormalization cx0 (Let bins0 env0) = bindVars (L.length bins0) withVars
  where
    bnames = fmap letBindingName bins0
    withVars bvars = Flows.bind (inferTypesOfTemporaryLetBindings cx1 bins0) withInferredBindings
      where
        tbins0 = fmap TypeVariable bvars
        cx1 = extendContext (L.zip bnames $ fmap Types.mono tbins0) cx0
        withInferredBindings (bterms1, tbins1, s1) = Flows.bind
            (unifyTypeLists (inferenceContextSchemaTypes cx0) (fmap (substInType s1) tbins0) tbins1 "temporary type bindings")
            withSubst
          where
            withSubst s2 = Flows.bind (inferTypeOfTerm (extendContext tsbins1 g2) env0 "let environment") withEnv
              where
                g2 = substInContext (composeTypeSubst s1 s2) cx0
                tsbins1 = L.zip bnames $ fmap (generalize g2 . substInType s2) tbins1
                withEnv (InferenceResult env1 tenv senv) = do
                    -- TODO: check type
                    return ret
                  where
                    st1 = TermSubst $ M.fromList $
                      fmap (\(name, ww) -> (name, (Terms.typeApplication (TermVariable name) $ fmap TypeVariable $ typeSchemeVariables ww))) tsbins1
                    -- TODO: should it be composeTypeSubst s2 senv, or even simply s2? We need tests for inferred types in terms.
                    bins1 = fmap (\((name, ts), term) -> LetBinding name
                        (substTypesInTerm (composeTypeSubst senv s2) $ Terms.typeAbstraction (typeSchemeVariables ts) $ substituteInTerm st1 term)
                        (Just $ substInTypeScheme senv ts))
                      $ zip tsbins1 bterms1
                    ret = InferenceResult (TermLet $ Let bins1 env1) tenv (composeTypeSubstList [s1, s2, senv])

inferTypeOfList :: InferenceContext -> [Term] -> Flow s InferenceResult
inferTypeOfList cx = inferTypeOfCollection cx Types.list Terms.list "list element"

inferTypeOfLiteral :: InferenceContext -> Literal -> Flow s InferenceResult
inferTypeOfLiteral _ lit = Flows.pure $ InferenceResult (TermLiteral lit) (TypeLiteral $ literalType lit) idTypeSubst

inferTypeOfMap :: InferenceContext -> M.Map Term Term -> Flow s InferenceResult
inferTypeOfMap cx m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        -- TODO: get rid of this special case; it should follow from the generate case
        then Flows.pure $ yield (TermMap M.empty) (Types.map (TypeVariable kvar) (TypeVariable vvar)) idTypeSubst
        else Flows.bind (inferMany cx $ fmap (\k -> (k, "map key")) $ M.keys m) withKeys
      where
        withKeys (kterms, ktypes, ksubst) = Flows.bind (inferMany cx $ fmap (\v -> (v, "map value")) $ M.elems m) withValues
          where
            withValues (vterms, vtypes, vsubst) = mapConstraints cx withSubst $ kcons ++ vcons
              where
                kcons = fmap (\t -> TypeConstraint (TypeVariable kvar) t "map key") ktypes
                vcons = fmap (\t -> TypeConstraint (TypeVariable vvar) t "map value") vtypes
                withSubst subst = yield
                    (TermMap $ M.fromList $ L.zip kterms vterms)
                    (Types.map (TypeVariable kvar) (TypeVariable vvar))
                    (composeTypeSubstList [ksubst, vsubst, subst])

inferTypeOfOptional :: InferenceContext -> Maybe Term -> Flow s InferenceResult
inferTypeOfOptional cx m = inferTypeOfCollection cx Types.optional trmCons "optional element" $ Y.maybe [] (\e -> [e]) m
  where
    trmCons terms = case terms of
      [] -> Terms.optional Nothing
      [term] -> Terms.optional $ Just term

inferTypeOfPrimitive :: InferenceContext -> Name -> Flow s InferenceResult
inferTypeOfPrimitive cx name = case M.lookup name (inferenceContextPrimitiveTypes cx) of
    Nothing -> Flows.fail $ "No such primitive: " ++ unName name
    Just scheme -> Flows.bind (instantiateTypeScheme scheme) withScheme
  where
    withScheme ts = yieldChecked cx (typeSchemeVariables ts) iterm itype idTypeSubst
      where
        iterm = L.foldl (\t v -> TermTypeApplication $ TypedTerm t $ TypeVariable v) (Terms.primitive name) $ typeSchemeVariables ts
        itype = typeSchemeType ts

inferTypeOfProduct :: InferenceContext -> [Term] -> Flow s InferenceResult
inferTypeOfProduct cx els = Flows.map withResults (inferMany cx $ fmap (\e -> (e, "tuple element")) els)
  where
    withResults (iterms, itypes, isubst) = yield (Terms.tuple iterms) (Types.product itypes) isubst

inferTypeOfProjection :: InferenceContext -> Projection -> Flow s InferenceResult
inferTypeOfProjection cx (Projection tname fname) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (Expect.recordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ findFieldType fname sfields
          where
            withField ftyp = yield
              (Terms.project tname fname)
              (Types.function (nominalApplication tname $ fmap TypeVariable svars) ftyp)
              idTypeSubst

inferTypeOfRecord :: InferenceContext -> Record -> Flow s InferenceResult
inferTypeOfRecord cx (Record tname fields) =
    bind2 (requireSchemaType cx tname) (inferMany cx $ fmap (\f -> (fieldTerm f, "field " ++ unName (fieldName f))) fields) withResults
  where
    fnames = fmap fieldName fields
    withResults (TypeScheme svars styp) (iterms, itypes, isubst) = mapConstraints cx withSubst [
        TypeConstraint styp ityp "schema type of record"]
      where
        ityp = TypeRecord $ RowType tname $ L.zipWith FieldType fnames itypes
        withSubst subst = yield
            (TermRecord $ Record tname $ L.zipWith Field fnames iterms)
            (nominalApplication tname $ fmap TypeVariable svars)
            (composeTypeSubst isubst subst)

inferTypeOfSet :: InferenceContext -> S.Set Term -> Flow s InferenceResult
inferTypeOfSet cx = inferTypeOfCollection cx Types.set (Terms.set . S.fromList) "set element" . S.toList

inferTypeOfSum :: InferenceContext -> Sum -> Flow s InferenceResult
inferTypeOfSum cx (Sum i s term) = bindInferredTerm cx term "sum term" withResult
  where
    withResult (InferenceResult iterm ityp isubst) = Flows.map withVars (Flows.sequence $ fmap (varOrTerm ityp) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then Flows.pure $ Left t
          else Right <$> freshName
        withVars vars = yield (TermSum $ Sum i s iterm) (TypeSum $ fmap toType vars) isubst
          where
            toType e = case e of
              Left t -> t
              Right v -> TypeVariable v

inferTypeOfTerm :: InferenceContext -> Term -> String -> Flow s InferenceResult
inferTypeOfTerm cx term desc = withTrace desc $ case term of
  TermAnnotated a -> inferTypeOfAnnotatedTerm cx a
  TermApplication a -> inferTypeOfApplication cx a
  TermFunction f -> inferTypeOfFunction cx f
  TermLet l -> inferTypeOfLet cx l
  TermList els -> inferTypeOfList cx els
  TermLiteral l -> inferTypeOfLiteral cx l
  TermMap m -> inferTypeOfMap cx m
  TermOptional m -> inferTypeOfOptional cx m
  TermProduct els -> inferTypeOfProduct cx els
  TermRecord r -> inferTypeOfRecord cx r
  TermSet s -> inferTypeOfSet cx s
  TermSum s -> inferTypeOfSum cx s
  TermTypeAbstraction ta -> inferTypeOfTypeAbstraction cx ta
  TermTypeApplication tt -> inferTypeOfTypeApplication cx tt
  TermTyped t -> inferTypeOfTypedTerm cx t
  TermUnion i -> inferTypeOfInjection cx i
  TermVariable name -> inferTypeOfVariable cx name
  TermWrap w -> inferTypeOfWrappedTerm cx w

inferTypeOfTupleProjection :: InferenceContext -> TupleProjection -> Flow s InferenceResult
inferTypeOfTupleProjection _ (TupleProjection arity idx _) = forVars arity withVars
  where
    withVars vars = yield
        (Terms.untuple arity idx $ Just types)
        (Types.function (Types.product types) cod)
        idTypeSubst
      where
        types = TypeVariable <$> vars
        cod = types !! idx

inferTypeOfTypeAbstraction :: InferenceContext -> TypeAbstraction -> Flow s InferenceResult
inferTypeOfTypeAbstraction cx (TypeAbstraction _ term) = inferTypeOfTerm cx term "type abstraction"

inferTypeOfTypeApplication :: InferenceContext -> TypedTerm -> Flow s InferenceResult
inferTypeOfTypeApplication cx (TypedTerm term _) = inferTypeOfTerm cx term "type application term"

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: InferenceContext -> TypedTerm -> Flow s InferenceResult
inferTypeOfTypedTerm cx (TypedTerm term _) = inferTypeOfTerm cx term "typed term"

inferTypeOfUnwrap :: InferenceContext -> Name -> Flow s InferenceResult
inferTypeOfUnwrap cx tname = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.map withWrappedType (Expect.wrappedType tname styp)
      where
        withWrappedType wtyp = yield
          (Terms.unwrap tname)
          (Types.function (nominalApplication tname $ fmap TypeVariable svars) wtyp)
          idTypeSubst

inferTypeOfVariable :: InferenceContext -> Name -> Flow s InferenceResult
inferTypeOfVariable cx name = case M.lookup name (inferenceContextDataTypes cx) of
    Nothing -> Flows.fail $ "Variable not bound to type: " ++ unName name
    Just scheme -> Flows.bind (instantiateTypeScheme scheme) withTypeScheme
  where
    withTypeScheme (TypeScheme vars itype) = do
--        checkType (S.fromList vars) cx itype iterm
        return $ InferenceResult iterm itype idTypeSubst
      where
        iterm = Terms.typeApplication (TermVariable name) $ fmap TypeVariable vars

inferTypeOfWrappedTerm :: InferenceContext -> WrappedTerm -> Flow s InferenceResult
inferTypeOfWrappedTerm cx (WrappedTerm tname term) =
  bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "wrapped term") withResult
  where
    withResult (TypeScheme svars styp) (InferenceResult iterm ityp isubst) = do
      freshVars <- freshNames (length svars)
      let subst = TypeSubst (M.fromList (zip svars (fmap TypeVariable freshVars)))
          stypInst = substInType subst styp
          nominalInst = nominalApplication tname $ fmap TypeVariable freshVars
          expected = TypeWrap (WrappedType tname ityp)
          freeVars = S.toList $
            freeVariablesInType ityp `S.union`
            freeVariablesInTerm iterm `S.union`
            S.fromList freshVars

      bindConstraints cx
        (\subst2 ->
           yieldChecked cx freeVars
             (Terms.wrap tname iterm)
             nominalInst
             (composeTypeSubst isubst subst2))
        [TypeConstraint stypInst expected "schema type of wrapper"]

inferTypesOfTemporaryLetBindings :: InferenceContext -> [LetBinding] -> Flow s ([Term], [Type], TypeSubst)
inferTypesOfTemporaryLetBindings cx bins = case bins of
  [] -> return ([], [], idTypeSubst)
  ((LetBinding k v _):tl) -> do
    InferenceResult j u' u <- inferTypeOfTerm cx v $ "temporary let binding '" ++ unName k ++ "'"
    (h, r', r) <- inferTypesOfTemporaryLetBindings (substInContext u cx) tl
    let ret = ((substTypesInTerm r j):h, (substInType r u'):r', composeTypeSubst u r)
    --check0 (subst (fst ret) g) (fst $ snd $ ret) (snd $ snd $ ret)
    return ret

--------------------------------------------------------------------------------

bindConstraints :: InferenceContext -> (TypeSubst -> Flow s a) -> [TypeConstraint] -> Flow s a
bindConstraints cx f constraints = Flows.bind (unifyTypeConstraints (inferenceContextSchemaTypes cx) constraints) f

bindVar :: (Name -> Flow s a) -> Flow s a
bindVar = Flows.bind freshName

bindVar2 :: (Name -> Name -> Flow s a) -> Flow s a
bindVar2 f = bindVar $ \v1 -> bindVar $ \v2 -> f v1 v2

bindVars :: Int -> ([Name] -> Flow s a) -> Flow s a
bindVars n = Flows.bind $ freshNames n

bindInferredTerm :: InferenceContext -> Term -> String -> (InferenceResult -> Flow s a) -> Flow s a
bindInferredTerm cx term desc = Flows.bind $ inferTypeOfTerm cx term desc

forInferredTerm :: InferenceContext -> Term -> String -> (InferenceResult -> a) -> Flow s a
forInferredTerm cx term desc f = Flows.map f $ inferTypeOfTerm cx term desc

forVar :: (Name -> a) -> Flow s a
forVar f = Flows.map f freshName

forVars :: Int -> ([Name] -> a) -> Flow s a
forVars n f = Flows.map f $ freshNames n

fTypeToTypeScheme :: Type -> TypeScheme
fTypeToTypeScheme typ = gather [] typ
  where
    gather vars typ = case stripType typ of
      TypeForall (ForallType v body) -> gather (v:vars) body
      t -> TypeScheme (L.reverse vars) t

instantiateFType :: Type -> Flow s Type
instantiateFType typ = do
  TypeScheme _ t <- instantiateTypeScheme (fTypeToTypeScheme typ)
  return t

instantiateTypeScheme :: TypeScheme -> Flow s TypeScheme
instantiateTypeScheme scheme = Flows.map doSubst (freshNames $ L.length oldVars)
  where
    doSubst newVars = TypeScheme newVars $ substInType subst $ typeSchemeType scheme
      where
        subst = TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)
    oldVars = typeSchemeVariables scheme

mapConstraints :: InferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a
mapConstraints cx f constraints = Flows.map f $ unifyTypeConstraints (inferenceContextSchemaTypes cx) constraints

nominalApplication :: Name -> [Type] -> Type
nominalApplication tname args = L.foldl (\t a -> Types.apply t a) (TypeVariable tname) args

requireSchemaType :: InferenceContext -> Name -> Flow s TypeScheme
requireSchemaType cx tname = case M.lookup tname (inferenceContextSchemaTypes cx) of
    Nothing -> Flows.fail $ "No such schema type: " ++ unName tname
    Just ts -> instantiateTypeScheme $ stripTypeSchemeRecursive ts

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: [(Name, TypeScheme)] -> InferenceContext -> InferenceContext
extendContext pairs cx = cx {inferenceContextDataTypes = M.union (M.fromList pairs) (inferenceContextDataTypes cx)}

yield :: Term -> Type -> TypeSubst -> InferenceResult
yield term typ subst = InferenceResult (substTypesInTerm subst term) (substInType subst typ) subst

yieldChecked :: InferenceContext -> [Name] -> Term -> Type -> TypeSubst -> Flow s InferenceResult
yieldChecked cx vars term typ subst = do
--    checkType (S.fromList vars) cx itype iterm
    return $ InferenceResult iterm itype subst
  where
    iterm = substTypesInTerm subst term
    itype = substInType subst typ

yieldDebug :: InferenceContext -> String -> Term -> Type -> TypeSubst -> Flow s InferenceResult
yieldDebug cx debugId term typ subst = do

    debugIf debugId $ ""
        ++ "\n\tterm: " ++ showTerm term
        ++ "\n\ttyp: " ++ showType typ
        ++ "\n\tsubst: " ++ showTypeSubst subst
        ++ "\n\trterm: " ++ showTerm rterm
        ++ "\n\trtyp: " ++ showType rtyp

    return $ InferenceResult rterm rtyp subst
  where
    rterm = substTypesInTerm subst term
    rtyp = substInType subst typ
