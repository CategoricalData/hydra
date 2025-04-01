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

type Types = M.Map Name Type

typeOf :: S.Set Name -> Types -> Term -> Flow Graph Type
typeOf vars types term = case term of
    TermAnnotated (AnnotatedTerm term1 _) -> typeOf vars types term1
    TermApplication (Application a b) -> do
      t1 <- typeOf vars types a
      t2 <- typeOf vars types b
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
          t1 <- typeOf vars (M.insert x t types) e
          checkTypeVariables vars t1
          return $ Types.function t t1
      FunctionPrimitive name -> typeSchemeToFType . primitiveType <$> requirePrimitive name -- Note: no instantiation
    TermLet (Let es e) -> do
        btypes <- CM.mapM binType es
        let types2 = M.union (M.fromList $ L.zip bnames btypes) types
        est <- CM.mapM (\v -> typeOf vars types2 v) bterms
        CM.mapM (checkTypeVariables vars) est
        CM.mapM (checkTypeVariables vars) btypes
        if est == btypes
          then typeOf vars types2 e
          else Flows.fail $ "binding types disagree: " ++ show (fmap showType est) ++ " and " ++ show (fmap showType btypes)
      where
        bnames = fmap letBindingName es
        bterms = fmap letBindingTerm es
        binType b = case letBindingType b of
          Nothing -> Flows.fail $ "untyped let binding in " ++ showTerm term
          Just ts -> return $ typeSchemeToFType ts
    TermList els -> typeOfCollection "list" TypeList vars types els
    TermLiteral lit -> return $ TypeLiteral $ literalType lit
    TermMap m -> if M.null m
        then return $ typeSchemeToFType $ Types.scheme ["k", "v"] $ Types.map (Types.var "k") (Types.var "v")
        else do
          kt <- (CM.mapM (typeOf vars types) $ fmap fst pairs) >>= singleType "map keys"
          vt <- (CM.mapM (typeOf vars types) $ fmap snd pairs) >>= singleType "map values"
          checkTypeVariables vars kt
          checkTypeVariables vars vt
          return $ TypeMap $ MapType kt vt
      where
        pairs = M.toList m
    TermOptional mt -> typeOfCollection "optional" TypeOptional vars types $ Y.maybe [] (\x -> [x]) mt
    TermProduct tuple -> do
      etypes <- CM.mapM (typeOf vars types) tuple
      CM.mapM (checkTypeVariables vars) etypes
      return $ TypeProduct etypes
--    TermRecord (Record tname fields) -> ...
    TermSet els -> typeOfCollection "set" TypeSet vars types $ S.toList els
--    TermSum (Sum idx size term1) -> ...
    TermTypeAbstraction (TypeAbstraction v e) -> do
      t1 <- typeOf (S.insert v vars) types e
      checkTypeVariables (S.insert v vars) t1
      return $ TypeForall $ ForallType v t1
    TermTypeApplication (TypedTerm e t) -> do
      t1 <- typeOf vars types e
      checkTypeVariables vars t1
      case t1 of
        TypeForall (ForallType v t2) -> return $ substInType (TypeSubst $ M.fromList [(v, t)]) t2
        t2 -> Flows.fail $ "not a forall type: " ++ showType t2
--    TermUnion (Injection tname (Field fname term1)) -> ...
    TermVariable name -> case M.lookup name types of
      Nothing -> Flows.fail $ "unbound variable: " ++ unName name
      Just t -> return t
--    TermWrap (WrappedTerm tname term1) -> ...
    _ -> Flows.fail $ "unexpected term variant: " ++ show (termVariant term)

typeOfCollection :: String -> (Type -> Type) -> S.Set Name -> Types -> [Term] -> Flow Graph Type
typeOfCollection desc cons vars types els = if L.null els
  then return $ typeSchemeToFType $ Types.scheme ["t"] $ cons $ Types.var "t"
  else do
    et <- CM.mapM (typeOf vars types) els >>= singleType desc
    checkTypeVariables vars et
    return $ cons et

singleType :: String -> [Type] -> Flow s Type
singleType desc types = if (L.foldl (\b t -> b && t == h) True types)
    then return h
    else Flows.fail $ "unequal types " ++ show (fmap showType types) ++ " in " ++ desc
  where
    h = L.head types

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
    schemaTypes <- case graphSchema g0 of
      Nothing -> Flows.fail "no schema provided"
      Just s -> schemaGraphToTypingEnvironment s
    return $ InferenceContext schemaTypes primTypes varTypes False
  where
    primTypes = M.fromList $ fmap (\p -> (primitiveName p, primitiveType p)) (M.elems $ graphPrimitives g0)
    varTypes = M.empty

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

-- TODO: deprecated (and expensive)
inferTermType :: Term -> Flow Graph Term
inferTermType term0 = do
  g <- getState
  cx <- graphToInferenceContext g
  fst <$> inferTypeOf cx term0

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
                        (TypeFunction $ FunctionType (nominalApplication tname svars) cod)
                        (composeTypeSubstList $ (optionalToList $ fmap inferenceResultSubst mr) ++ [isubst, subst])

inferTypeOfCollection :: InferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s InferenceResult
inferTypeOfCollection cx typCons trmCons desc els = bindVar withVar
  where
    withVar var = Flows.bind (inferMany cx $ L.zip els $ fmap (\i -> "#" ++ show i) [1..(L.length els)]) fromResults
      where
        fromResults (terms, types, subst1) = mapConstraints cx withConstraints $
            fmap (\t -> TypeConstraint (TypeVariable var) t desc) types
          where
            withConstraints subst2 = yield (trmCons terms) (typCons $ TypeVariable var) $ composeTypeSubst subst1 subst2

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
                  (nominalApplication tname svars)
                  (composeTypeSubst isubst subst)

inferTypeOfLambda :: InferenceContext -> Lambda -> Flow s InferenceResult
inferTypeOfLambda cx tmp@(Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = Flows.map withResult (inferTypeOfTerm cx2 body "lambda body")
      where
        dom = TypeVariable vdom
        cx2 = extendContext [(var, Types.mono $ TypeVariable vdom)] cx
        withResult (InferenceResult iterm itype isubst) =
          yield (TermFunction $ FunctionLambda $ Lambda var (Just dom) iterm) (Types.function dom itype) isubst

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
    -- TODO: check against algo W implementation
    Just scheme -> Flows.map withScheme $ instantiateTypeScheme scheme
  where
    withScheme ts = yield (Terms.primitive name) (typeSchemeType ts) idTypeSubst

inferTypeOfProduct :: InferenceContext -> [Term] -> Flow s InferenceResult
inferTypeOfProduct cx els = Flows.map withResults (inferMany cx $ fmap (\e -> (e, "tuple element")) els)
  where
    withResults (iterms, itypes, isubst) = yield (Terms.product iterms) (Types.product itypes) isubst

inferTypeOfProjection :: InferenceContext -> Projection -> Flow s InferenceResult
inferTypeOfProjection cx (Projection tname fname) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (Expect.recordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ findFieldType fname sfields
          where
            withField ftyp = yield
              (Terms.project tname fname)
              (Types.function (nominalApplication tname svars) ftyp)
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
            (nominalApplication tname svars)
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
          (Types.function (nominalApplication tname svars) wtyp)
          idTypeSubst

inferTypeOfVariable :: InferenceContext -> Name -> Flow s InferenceResult
inferTypeOfVariable cx name = case M.lookup name (inferenceContextDataTypes cx) of
    Nothing -> Flows.fail $ "Variable not bound to type: " ++ unName name
    Just scheme -> Flows.map withTypeScheme $ instantiateTypeScheme scheme
  where
    withTypeScheme (TypeScheme vars typ) = InferenceResult
      (Terms.typeApplication (TermVariable name) $ fmap TypeVariable vars)
      typ
      idTypeSubst

inferTypeOfWrappedTerm :: InferenceContext -> WrappedTerm -> Flow s InferenceResult
inferTypeOfWrappedTerm cx (WrappedTerm tname term) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "wrapped term") withResult
  where
    withResult (TypeScheme svars styp) (InferenceResult iterm ityp isubst)
        = mapConstraints cx withSubst [
          TypeConstraint styp (TypeWrap $ WrappedType tname ityp) "schema type of wrapper"]
      where
        withSubst subst = yield (Terms.wrap tname iterm) (nominalApplication tname svars) (composeTypeSubst isubst subst)

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

instantiateTypeScheme :: TypeScheme -> Flow s TypeScheme
instantiateTypeScheme scheme = Flows.map doSubst (freshNames $ L.length oldVars)
  where
    doSubst newVars = TypeScheme newVars $ substInType subst $ typeSchemeType scheme
      where
        subst = TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)
    oldVars = typeSchemeVariables scheme

mapConstraints :: InferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a
mapConstraints cx f constraints = Flows.map f $ unifyTypeConstraints (inferenceContextSchemaTypes cx) constraints

nominalApplication :: Name -> [Name] -> Type
nominalApplication tname vars = L.foldl (\t v -> Types.apply t $ TypeVariable v) (TypeVariable tname) vars

requireSchemaType :: InferenceContext -> Name -> Flow s TypeScheme
requireSchemaType cx tname = case M.lookup tname (inferenceContextSchemaTypes cx) of
    Nothing -> Flows.fail $ "No such schema type: " ++ unName tname
    Just ts -> instantiateTypeScheme $ stripTypeSchemeRecursive ts

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: [(Name, TypeScheme)] -> InferenceContext -> InferenceContext
extendContext pairs cx = cx {inferenceContextDataTypes = M.union (M.fromList pairs) (inferenceContextDataTypes cx)}

yield :: Term -> Type -> TypeSubst -> InferenceResult
yield term typ subst = InferenceResult (substTypesInTerm subst term) (substInType subst typ) subst

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
