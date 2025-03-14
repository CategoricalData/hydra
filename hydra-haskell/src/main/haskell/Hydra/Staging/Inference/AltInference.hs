module Hydra.Staging.Inference.AltInference where

import Hydra.Coders
import Hydra.Compute
import Hydra.Core
import Hydra.Errors
import Hydra.Flows
import Hydra.Graph
import Hydra.Lib.Flows as Flows
import Hydra.Lib.Io
import Hydra.Mantle
import Hydra.Rewriting
import Hydra.Staging.Annotations
import Hydra.Staging.CoreDecoding
import Hydra.Staging.Rewriting
import Hydra.Staging.Schemas
import Hydra.Staging.Sorting
import Hydra.Strip
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
key_debugId = Name "debugId"

debugIf :: String -> String -> Flow s ()
debugIf debugId message = do
  desc <- getDebugId
  if desc == Just debugId
    then Flows.fail message
    else return ()

getDebugId :: Flow s (Maybe String)
getDebugId = do
  desc <- getAttr key_debugId
  traverse Expect.string desc

freshName :: Flow s Name
freshName = normalTypeVariable <$> nextCount key_vcount

freshNames :: Int -> Flow s [Name]
freshNames n = Flows.sequence $ L.replicate n freshName

freshVariableType :: Flow s Type
freshVariableType = TypeVariable <$> freshName

normalTypeVariables :: [Name]
normalTypeVariables = normalTypeVariable <$> [0..]

-- | Type variable naming convention follows Haskell: t0, t1, etc.
normalTypeVariable :: Int -> Name
normalTypeVariable i = Name $ "t" ++ show i

--------------------------------------------------------------------------------
-- Unification

-- | Determine whether a type variable appears within a type expression.
--   No distinction is made between free and bound type variables.
variableOccursInType :: Name -> Type -> Bool
variableOccursInType var = foldOverType TraversalOrderPre tryType False
  where
    tryType b typ = case typ of
      TypeVariable v -> b || v == var
      _ -> b

{-
Robinson's algorithm, following https://www.cs.cornell.edu/courses/cs6110/2017sp/lectures/lec23.pdf
Specifically this is an implementation of the following rules:
 * Unify({(x, t)} ∪ E) = {t/x} Unify(E{t/x}) if x ∉ FV(t)
 * Unify(∅) = I (the identity substitution x ↦ x)
 * Unify({(x, x)} ∪ E) = Unify(E)
 * Unify({(f(s1, ..., sn), f(t1, ..., tn))} ∪ E) = Unify({(s1, t1), ..., (sn, tn)} ∪ E)
-}
unifyTypeConstraints :: AltInferenceContext -> [TypeConstraint] -> Flow s TypeSubst
unifyTypeConstraints cx constraints = case constraints of
  [] -> Flows.pure emptyTypeSubst
  ((TypeConstraint left right comment):rest) -> do
      result <- case sleft of
       TypeVariable name -> case sright of
           TypeVariable name2 -> if name == name2
             then unifyTypeConstraints cx rest
             -- Avoid replacing schema type references with temporary type variables.
             else if Y.isJust (M.lookup name $ altInferenceContextSchemaTypes cx)
             then if Y.isJust (M.lookup name2 $ altInferenceContextSchemaTypes cx)
               then Flows.fail $ "attempted to unify schema names " ++ unName name ++ " and " ++ unName name2
               else bind name2 sleft
             else bind name sright
           _ -> tryBinding name sright
       _ -> case sright of
         TypeVariable name -> tryBinding name sleft
         _ -> do
           constraints2 <- joinTypes sleft sright comment
           unifyTypeConstraints cx $ constraints2 ++ rest
      return result
    where
      sleft = stripType left
      sright = stripType right
      -- TODO: this occurrence check is expensive; consider delaying it until the time of substitution
      tryBinding v t = if variableOccursInType v t
        then Flows.fail $ "Variable " ++ unName v ++ " appears free in type " ++ showType t
          ++ " (" ++ comment ++ ")"
        else bind v t
      bind v t = composeTypeSubst subst <$> unifyTypeConstraints cx (substituteInConstraints subst rest)
        where
          subst = singletonTypeSubst v t

unifyTypeLists :: AltInferenceContext -> [Type] -> [Type] -> String -> Flow s TypeSubst
unifyTypeLists cx l r comment = unifyTypeConstraints cx $ L.zipWith toConstraint l r
  where
    toConstraint l r = TypeConstraint l r comment

unifyTypes :: AltInferenceContext -> Type -> Type -> String -> Flow s TypeSubst
unifyTypes cx l r comment = unifyTypeConstraints cx [TypeConstraint l r comment]

joinTypes :: Type -> Type -> String -> Flow s [TypeConstraint]
joinTypes left right comment = case sleft of
    TypeApplication (ApplicationType lhs1 rhs1) -> case sright of
      TypeApplication (ApplicationType lhs2 rhs2) -> Flows.pure [
        joinOne lhs1 lhs2,
        joinOne rhs1 rhs2]
      _ -> cannotUnify
    TypeFunction (FunctionType domleft codleft) -> case sright of
      TypeFunction (FunctionType domright codright) -> Flows.pure [
        joinOne domleft domright,
        joinOne codleft codright]
    TypeList eleft -> case sright of
      TypeList eright -> Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    TypeLiteral ltleft -> assertEqual
    TypeMap (MapType kleft vleft) -> case sright of
      TypeMap (MapType kright vright) -> Flows.pure [
        joinOne kleft kright,
        joinOne vleft vright]
      _ -> cannotUnify
    TypeOptional eleft -> case sright of
      TypeOptional eright -> Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    TypeProduct lefts -> case sright of
      TypeProduct rights -> joinList lefts rights
      _ -> cannotUnify
    TypeRecord rtleft -> case sright of
      TypeRecord rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeSet eleft -> case sright of
      TypeSet eright -> Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    TypeSum lefts -> case sright of
      TypeSum rights -> joinList lefts rights
      _ -> cannotUnify
    TypeUnion rtleft -> case sright of
      TypeUnion rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeWrap (WrappedType nameLeft eleft) -> case sright of
      TypeWrap (WrappedType nameRight eright) -> if nameLeft /= nameRight
        then cannotUnify
        else Flows.pure [joinOne eleft eright]
      _ -> cannotUnify
    -- TypeAnnotated, TypeLambda, TypeVariable should not appear here
    _ -> cannotUnify
  where
    sleft = stripType left
    sright = stripType right
--    joinOne l r = TypeConstraint l r $ "join types; " ++ comment
    joinOne l r = TypeConstraint l r $ "join types " ++ showType l ++ " with " ++ showType r ++ "; " ++ comment
    cannotUnify = Flows.fail $ "Cannot unify " ++ showType sleft ++ " with " ++ showType sright
    assertEqual = if sleft == sright
      then Flows.pure []
      else cannotUnify
    joinList lefts rights = if L.length lefts == L.length rights
      then Flows.pure $ L.zipWith joinOne lefts rights
      else cannotUnify
    fieldNames = fmap fieldTypeName
    fieldTypes = fmap fieldTypeType
    joinRowTypes (RowType tnameLeft fieldsLeft) (RowType tnameRight fieldsRight) = if tnameLeft /= tnameRight
      then cannotUnify
      else if (fieldNames fieldsLeft) /= (fieldNames fieldsRight)
      then cannotUnify
      else joinList (fieldTypes fieldsLeft) (fieldTypes fieldsRight)

--------------------------------------------------------------------------------
-- Substitution

-- | The composition S T of two substitution is the result of first applying S, then T.
composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst s1 s2 =
    TypeSubst $ M.union addExtra $ fmap (substInType s2) $ unTypeSubst s1
  where
    addExtra = M.filterWithKey (\v _ -> Y.isNothing $ M.lookup v $ unTypeSubst s1) $ unTypeSubst s2

composeTypeSubstList :: [TypeSubst] -> TypeSubst
composeTypeSubstList = L.foldl composeTypeSubst emptyTypeSubst

emptyTypeSubst = TypeSubst M.empty
tmpTypeSubst = TypeSubst M.empty

singletonTypeSubst :: Name -> Type -> TypeSubst
singletonTypeSubst v t = TypeSubst $ M.singleton v t

substituteInTerm :: TermSubst -> Term -> Term
substituteInTerm (TermSubst s) = rewriteTerm rewrite
  where
    rewrite recurse term = case term of
      TermFunction (FunctionLambda (Lambda v mt body)) ->
          TermFunction $ FunctionLambda $ Lambda v mt $ substituteInTerm subst2 body
        where
          subst2 = TermSubst $ M.delete v s
      TermLet (Let bindings env) -> TermLet $ Let (fmap rewriteBinding bindings) $ substituteInTerm subst2 env
        where
          names = fmap letBindingName bindings
          subst2 = TermSubst $ M.filterWithKey (\k _ -> not $ L.elem k names) s
          rewriteBinding (LetBinding name term mt) = LetBinding name (substituteInTerm subst2 term) mt
      TermVariable name -> case M.lookup name s of
        Just sterm -> sterm
        Nothing -> recurse term
      _ -> recurse term

substInContext :: TypeSubst -> AltInferenceContext -> AltInferenceContext
substInContext subst cx = cx {
  altInferenceContextDataTypes = fmap (substInTypeScheme subst) $ altInferenceContextDataTypes cx}

substInType :: TypeSubst -> Type -> Type
substInType subst = rewriteType rewrite
  where
    rewrite recurse typ = case typ of
      TypeLambda (LambdaType v body) -> case M.lookup v (unTypeSubst subst) of
        Nothing -> recurse typ
        Just styp -> TypeLambda $ LambdaType v $ substInType subst2 body
          where
            subst2 = TypeSubst $ M.delete v $ unTypeSubst subst
      TypeVariable v -> case M.lookup v (unTypeSubst subst) of
        Nothing -> typ
        Just styp -> styp
      _ -> recurse typ

substInTypeScheme :: TypeSubst -> TypeScheme -> TypeScheme
substInTypeScheme subst (TypeScheme vars typ) = TypeScheme vars $ substInType subst2 typ
  where
    subst2 = TypeSubst $ M.filterWithKey (\k _ -> not $ k `elem` vars) $ unTypeSubst subst

substInTypeSchemeLegacy :: TypeSubst -> TypeScheme -> TypeScheme
substInTypeSchemeLegacy subst (TypeScheme vars typ) = TypeScheme vars $ substInType subst typ

substituteInConstraint :: TypeSubst -> TypeConstraint -> TypeConstraint
substituteInConstraint subst (TypeConstraint t1 t2 ctx) = TypeConstraint (substInType subst t1) (substInType subst t2) ctx

substituteInConstraints :: TypeSubst -> [TypeConstraint] -> [TypeConstraint]
substituteInConstraints subst = fmap (substituteInConstraint subst)

substTypesInTerm :: TypeSubst -> Term -> Term
substTypesInTerm subst = rewriteTerm rewrite
  where
    rewrite recurse term = case term of
      -- TODO: injections and case statements need a domain field as well, similar to lambdas
      TermFunction (FunctionLambda (Lambda v mt body)) -> recurse $ TermFunction $ FunctionLambda $ Lambda v (fmap (substInType subst) mt) body
      TermLet (Let bindings env) -> recurse $ TermLet $ Let (fmap rewriteBinding bindings) env
        where
          rewriteBinding (LetBinding v e mt) = LetBinding v e $ fmap (substInTypeScheme subst) mt
      TermTypeAbstraction (TypeAbstraction param body) -> TermTypeAbstraction $ TypeAbstraction param $ substTypesInTerm subst2 body
        where
          subst2 = TypeSubst $ M.delete param $ unTypeSubst subst
      TermTypeApplication (TypedTerm trm typ) -> recurse $ TermTypeApplication $ TypedTerm trm $ substInType subst typ
      _ -> recurse term

--------------------------------------------------------------------------------
-- Type checking

--typeOf :: AltInferenceContext -> Term -> Flow s Type
--typeOf cx term = case term of
----    TermAnnotated (AnnotatedTerm term1 _) -> ...
----    TermApplication (Application lhs rhs) -> ...
----    TermFunction f -> case f of
----      FunctionElimination elm -> ...
----      FunctionLambda (Lambda v mt body) -> ...
----      FunctionPrimitive name -> ...
----    TermLet (Let bindings env) -> ...
--    TermList els -> do
--      types <- CM.mapM (typeOf cx) els
--      TypeList <$> typeOf cx
--    TermLiteral lit -> Flows.pure $ TypeLiteral $ literalType lit
----    TermMap m -> ...
----    TermOptional mt -> ...
----    TermProduct tuple -> ...
----    TermRecord (Record tname fields) -> ...
----    TermSet s -> ...
----    TermSum (Sum idx size term1) -> ...
----    TermTypeAbstraction (TypeAbstraction param body) -> ...
----    TermTypeApplication (TypedTerm trm typ) -> ...
----    TermUnion (Injection tname (Field fname term1)) -> ...
----    TermVariable name -> ...
----    TermWrap (WrappedTerm tname term1) -> ...
--    _ -> Flows.fail $ "unexpected term variant: " ++ show (termVariant term)

--------------------------------------------------------------------------------
-- Inference

data AltInferenceContext
  = AltInferenceContext {
    -- | A fixed typing environment which is derived from the schema of the graph.
    altInferenceContextSchemaTypes :: M.Map Name TypeScheme,
    -- | A fixed typing environment which is derived from the set of primitives in the graph.
    altInferenceContextPrimitiveTypes :: M.Map Name TypeScheme,
    -- | A mutable typing environment which is specific to the current graph being processed.
    --   This environment is (usually) smaller than the schema and primitive typing environments,
    --   and is subject to global substitutions.
    altInferenceContextDataTypes :: M.Map Name TypeScheme,
    altInferenceContextDebug :: Bool}
  deriving (Eq, Ord, Show)

data AltInferenceResult
  = AltInferenceResult {
    altInferenceResultTerm :: Term,
    altInferenceResultType :: Type,
    altInferenceResultSubst :: TypeSubst}
  deriving (Eq, Ord)

showAltInferenceResult :: AltInferenceResult -> String
showAltInferenceResult (AltInferenceResult term typ subst) = "{"
    ++ "term=" ++ showTerm term ++ ", "
    ++ "type= " ++ showType typ ++ ", "
    ++ "subst= " ++ showTypeSubst subst ++ "}"

emptyInferenceContext :: AltInferenceContext
emptyInferenceContext = AltInferenceContext M.empty M.empty M.empty False

freeVariablesInContext :: AltInferenceContext -> S.Set Name
freeVariablesInContext cx = L.foldl S.union S.empty $ fmap freeVariablesInTypeSchemeSimple $ M.elems $ altInferenceContextDataTypes cx

generalize :: AltInferenceContext -> Type -> TypeScheme
generalize cx typ = TypeScheme vars typ
  where
    vars = L.nub $ L.filter isUnbound $ S.toList $ freeVariablesInType typ
    isUnbound v = not (S.member v $ freeVariablesInContext cx)
      && not (M.member v $ altInferenceContextSchemaTypes cx)

graphToInferenceContext :: Graph -> Flow s AltInferenceContext
graphToInferenceContext g0 = do
    schemaTypes <- case graphSchema g0 of
      Nothing -> Flows.fail "no schema provided"
      Just s -> schemaGraphToTypingEnvironment s
    return $ AltInferenceContext schemaTypes primTypes varTypes False
  where
    primTypes = M.fromList $ fmap (\p -> (primitiveName p, primitiveType p)) (M.elems $ graphPrimitives g0)
    varTypes = M.empty

inferGraphTypes :: Graph -> Flow s Graph
inferGraphTypes g0 = withTrace "graph inference" $ do
--    case graphSchema g0 of
--      Nothing -> Flows.fail "no schema"
--      Just s -> do
--        str <- showSchemaGraph s
--        Flows.fail $ "schema graph: " ++ str
    cx <- graphToInferenceContext g0
    Flows.bind (inferTypeOfTerm cx (toLetTerm g0) "graph term") withResult
  where
    toLetTerm g = TermLet $ Let (fmap toBinding $ M.elems $ graphElements g) $ graphBody g
      where
        toBinding (Element name term _) = LetBinding name term Nothing
    withResult (AltInferenceResult term ts _) = case normalizeTypeVariablesInTerm term of
      TermLet l -> Flows.pure $ fromLetTerm l
--      TermLet l -> Flows.fail $ "resulting graph: " ++ showTerm (TermLet l)
      _ -> Flows.fail $ "Expected inferred graph as let term"
    fromLetTerm (Let bindings env) = g0 {
        graphElements = M.fromList $ fmap fromBinding bindings,
        graphBody = env,
        graphEnvironment = M.empty,
        graphTypes = M.empty}
      where
        fromBinding (LetBinding name term mt) = (name, Element name term mt)
--    showSchemaGraph g = withState g $ do
--        s <- CM.mapM forEl typeEls
--        return $ "{" ++ (L.intercalate ", " s) ++ "}"
--      where
--        typeEls = L.filter isEncodedType $ M.elems $ graphElements g
--        isEncodedType el = case (fullyStripTerm $ elementTerm el) of
--          TermUnion (Injection tname _) -> tname == _Type
--          _ -> False
--        forEl el = do
--          typ <- coreDecodeType $ elementTerm el
--          return $ unName (elementName el) ++ ": " ++ showType typ

inferMany :: AltInferenceContext -> [(Term, String)] -> Flow s ([Term], [Type], TypeSubst)
inferMany cx pairs = case pairs of
  [] -> return ([], [], emptyTypeSubst)
  ((e, desc):tl) -> do
    (AltInferenceResult e1 t1 s1) <- inferTypeOfTerm cx e desc
    (e2, t2, s2) <- inferMany (substInContext s1 cx) tl
    return (
      (substTypesInTerm s2 e1):e2,
      (substInType s2 t1):t2,
      composeTypeSubst s1 s2)

inferAndBindMany :: AltInferenceContext -> [(Name, Term)] -> Flow s ([Term], [Type], TypeSubst, AltInferenceContext)
inferAndBindMany cx pairs = case pairs of
  [] -> return ([], [], emptyTypeSubst, cx)
  ((name, e):tl) -> do
    (AltInferenceResult e1 t1 s1) <- inferTypeOfTerm cx e $ "infer and bind " ++ unName name
    let cx1 = substInContext s1 cx
    let cx2 = extendContext [(name, generalize cx1 t1)] cx1
    (e2, t2, s2, cx3) <- inferAndBindMany cx2 tl
    return (
      (substTypesInTerm s2 e1):e2,
      (substInType s2 t1):t2,
      composeTypeSubst s1 s2,
      cx3)

-- TODO: deprecated (and expensive)
inferTermType :: Term -> Flow Graph Term
inferTermType term0 = do
  g <- getState
  cx <- graphToInferenceContext g
  fst <$> inferTypeOf cx term0

inferTwo :: AltInferenceContext -> Term -> String -> Term -> String -> Flow s (Term, Type, Term, Type, TypeSubst)
inferTwo cx term1 desc1 term2 desc2 = Flows.map withResult $ inferMany cx [(term1, desc1), (term2, desc2)]
  where
    withResult ([e1, e2], [t1, t2], s) = (e1, t1, e2, t2, s)
    withResult _ = error "unexpected result from inferMany"

inferTypeOf :: AltInferenceContext -> Term -> Flow s (Term, TypeScheme)
inferTypeOf cx term = bindInferredTerm cx letTerm "infer type of term" unifyAndSubst
--inferTypeOf cx term = bindInferredTerm cx letTerm ("infer type of term: " ++ showTerm term) unifyAndSubst
  where
    letTerm = TermLet $ Let [LetBinding (Name "x") term Nothing] $ Terms.string "ignored"
    unifyAndSubst result = do
        (Let bindings _) <- Expect.letTerm $ normalizeTypeVariablesInTerm $ altInferenceResultTerm result
        case bindings of
          [LetBinding _ term1 (Just ts)] -> return (term1, ts)
          _ -> Flows.fail $ "Expected a single binding with a type scheme, but got: " ++ show bindings
      where
        subst = altInferenceResultSubst result

inferTypeOfElement :: AltInferenceContext -> Element -> Flow s Element
inferTypeOfElement cx (Element name term _) = withTrace ("inference on element " ++ unName name) $ do
  (term1, ts) <- inferTypeOf cx term
  return $ Element name term1 (Just ts)

-- TODO: deprecated (and expensive)
inferredTypeOf :: Term -> Flow Graph Type
inferredTypeOf term = do
  g <- getState
  cx <- graphToInferenceContext g
  typeSchemeType . snd <$> inferTypeOf cx term

inferTypeOfAnnotatedTerm :: AltInferenceContext -> AnnotatedTerm -> Flow s AltInferenceResult
inferTypeOfAnnotatedTerm cx (AnnotatedTerm term ann) = Flows.map withResult $ inferTypeOfTerm cx term "annotated term"
  where
    withResult (AltInferenceResult iterm itype isubst)
      = AltInferenceResult (TermAnnotated $ AnnotatedTerm iterm ann) itype isubst

inferTypeOfApplication :: AltInferenceContext -> Application -> Flow s AltInferenceResult
inferTypeOfApplication cx (Application e0 e1) = bindInferredTerm cx e0 "lhs" withLhs
  where
    withLhs (AltInferenceResult a t0 s0) = bindInferredTerm (substInContext s0 cx) e1 "rhs" withRhs
      where
        withRhs (AltInferenceResult b t1 s1) = bindVar withVar
          where
--            withVar v = Flows.map withSubst $ unifyTypes cx
            withVar v = withTrace ("applying " ++ showTerm e0 ++ " to " ++ showTerm e1 ++ "; types are " ++ showType t0 ++ " and " ++ showType t1 ) $ Flows.map withSubst $ unifyTypes cx
                (substInType s1 t0)
                (Types.function t1 $ TypeVariable v)
                "application lhs"
              where
                withSubst s2 = AltInferenceResult rExpr rType rSubst
                  where
                    rExpr = Terms.apply (substTypesInTerm (composeTypeSubst s1 s2) a) (substTypesInTerm s2 b)
                    rType = substInType s2 $ TypeVariable v
                    rSubst = composeTypeSubstList [s0, s1, s2]

inferTypeOfCaseStatement :: AltInferenceContext -> CaseStatement -> Flow s AltInferenceResult
inferTypeOfCaseStatement cx (CaseStatement tname dflt cases) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    fnames = fmap fieldName cases
    withSchemaType (TypeScheme svars styp) = Flows.bind (expectUnionType tname styp) withFields
      where
        withFields sfields = bind2 (traverse (\t -> inferTypeOfTerm cx t "default") dflt) (inferMany cx $ fmap (\f -> (fieldTerm f, "case " ++ unName (fieldName f))) cases) withResults
          where
            withResults mr (iterms, itypes, isubst) = bindVar withCod
              where
                withCod codv = mapConstraints cx withConstraints (dfltConstraints ++ caseConstraints)
                  where
                    cod = TypeVariable codv
                    dfltConstraints = maybeToList $ fmap (\r -> TypeConstraint cod (altInferenceResultType r) "match default") mr
                    caseConstraints = Y.catMaybes $ L.zipWith (\fname itype -> fmap (\r -> toConstraint r itype) $ M.lookup fname caseMap) fnames itypes
                      where
                        caseMap = M.fromList $ fmap (\(FieldType fname ftype) -> (fname, ftype)) sfields
                        toConstraint ftyp r = TypeConstraint r (Types.function ftyp cod) "case type"
                    withConstraints subst = yield
                        (TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement tname (fmap altInferenceResultTerm mr) $ L.zipWith Field fnames iterms)
                        (TypeFunction $ FunctionType (nominalApplication tname svars) cod)
                        (composeTypeSubstList $ (maybeToList $ fmap altInferenceResultSubst mr) ++ [isubst, subst])

inferTypeOfCollection :: AltInferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s AltInferenceResult
inferTypeOfCollection cx typCons trmCons desc els = bindVar withVar
  where
    withVar var = Flows.bind (inferMany cx $ L.zip els $ fmap (\i -> "#" ++ show i) [1..(L.length els)]) fromResults
      where
        fromResults (terms, types, subst1) = bindConstraints cx constraints withConstraints
          where
            constraints = fmap (\t -> TypeConstraint (TypeVariable var) t desc) types
            withConstraints subst2 = do

--                debugIf "All tests, Inference tests, Simple terms, List terms, List with bound variables, #1" $ ""
--                    ++ "\n\tels: " ++ (L.intercalate ", " $ fmap showTerm els)
--                    ++ "\n\trExpr: " ++ showTerm rExpr
--                    ++ "\n\trType: " ++ showType rType
--                    ++ "\n\tsubst1: " ++ show subst1
--                    ++ "\n\tsubst2: " ++ show subst2
--                    ++ "\n\tsubst: " ++ show subst

                return $ yield (trmCons terms) (typCons $ TypeVariable var) $ composeTypeSubst subst1 subst2

inferTypeOfElimination :: AltInferenceContext -> Elimination -> Flow s AltInferenceResult
inferTypeOfElimination cx elm = case elm of
  EliminationList fun -> inferTypeOfFold cx fun
  EliminationOptional oc -> inferTypeOfOptionalCases cx oc
  EliminationProduct tp -> inferTypeOfTupleProjection cx tp
  EliminationRecord p -> inferTypeOfProjection cx p
  EliminationUnion c -> inferTypeOfCaseStatement cx c
  EliminationWrap tname -> inferTypeOfUnwrap cx tname

inferTypeOfFold :: AltInferenceContext -> Term -> Flow s AltInferenceResult
inferTypeOfFold cx fun = bindVar2 withVars
  where
    withVars a b = Flows.bind (inferTypeOfTerm cx fun "fold function") withResult
      where
        aT = TypeVariable a
        bT = TypeVariable b
        withResult (AltInferenceResult iterm ityp isubst) = mapConstraints cx withSubst [
            TypeConstraint expectedType ityp "fold function"]
          where
            expectedType = Types.functionN [bT, aT, bT]
            withSubst subst = yield iterm (Types.functionN [bT, Types.list aT, bT]) $ composeTypeSubst isubst subst

inferTypeOfFunction :: AltInferenceContext -> Function -> Flow s AltInferenceResult
inferTypeOfFunction cx f = case f of
  FunctionElimination elm -> inferTypeOfElimination cx elm
  FunctionLambda l -> inferTypeOfLambda cx l
  FunctionPrimitive name -> inferTypeOfPrimitive cx name

inferTypeOfInjection :: AltInferenceContext -> Injection -> Flow s AltInferenceResult
inferTypeOfInjection cx (Injection tname (Field fname term)) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "injected term") withResults
  where
    withResults (TypeScheme svars styp) (AltInferenceResult iterm ityp isubst) =
        Flows.bind (expectUnionType tname styp) withFields
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

inferTypeOfLambda :: AltInferenceContext -> Lambda -> Flow s AltInferenceResult
inferTypeOfLambda cx tmp@(Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = Flows.map withResult (inferTypeOfTerm cx2 body "lambda body")
      where
        dom = TypeVariable vdom
        cx2 = extendContext [(var, Types.mono $ TypeVariable vdom)] cx
        withResult (AltInferenceResult iterm itype isubst) =
          yield (TermFunction $ FunctionLambda $ Lambda var (Just dom) iterm) (Types.function dom itype) isubst

-- | Normalize a let term before inferring its type.
inferTypeOfLet :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfLet cx lt@(Let bindings0 env0) = Flows.map rewriteResult $ case rewrittenLet of
     TermLet l -> inferTypeOfNormalizedLet cx l
     t -> inferTypeOfTerm cx t "empty let term"
  where
    names = fmap letBindingName bindings0
    groups = case topologicalSort adjList of
        Left g -> g
        Right l -> if L.null l then [] else [l]
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
    restoreLet iterm = TermLet $ Let (Y.catMaybes $ fmap (\n -> M.lookup n bindingMap) names) iterm
      where
        (bindingList, e) = helper (L.length groups) [] iterm
        bindingMap = M.fromList $ fmap (\b -> (letBindingName b, b)) bindingList
        helper level bins term = if level == 0
          then (bins, term)
          else case term of
            TermLet (Let bs e) -> helper (level - 1) (bs ++ bins) e
    rewriteResult result@(AltInferenceResult iterm itype isubst) = AltInferenceResult (restoreLet iterm) itype isubst

-- | Infer the type of a let (letrec) term which is already in a normal form.
inferTypeOfNormalizedLet :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfNormalizedLet cx (Let b0 env0) = bindVars (L.length b0) withVars
  where
    bnames = fmap letBindingName b0
    eb0 = fmap letBindingTerm b0
    withVars bvars = Flows.bind (inferAndBindMany cx1 $ L.zip bnames eb0) withInferredBindings
      where
        cx1 = extendContext (L.zip bnames $ fmap Types.mono btypes) cx
        btypes = fmap TypeVariable bvars
        withInferredBindings (eb1, tb, s1, cx2) = Flows.bind
            (unifyTypeLists cx2 (fmap (substInType s1) btypes) tb "let temporary type bindings") withSubst
          where
            withSubst sb = Flows.map withEnv (inferTypeOfTerm cx3 env0 "let environment")
              where
                cx3 = extendContext (L.zip bnames $ fmap (generalize cx2 . substInType sb) tb) cx2
                withEnv tmp@(AltInferenceResult env1 tenv senv) = AltInferenceResult
                    (TermLet $ Let b3 env1)
                    tenv
                    (composeTypeSubstList [s1, sb, senv])
                  where
                    eb2 = fmap (substTypesInTerm $ composeTypeSubst sb senv) eb1
                    b3t = L.zip bnames $ fmap (generalize cx . substInType sb) tb
                    s3 = TermSubst $ M.fromList $ fmap (\(x, ts) -> (x, (Terms.typeApplication (TermVariable x) $ fmap TypeVariable $ typeSchemeVariables ts))) b3t
                    b3 = L.zipWith (\(x, ts) e -> LetBinding x
                      (substTypesInTerm (composeTypeSubst senv sb) $ Terms.typeAbstraction (typeSchemeVariables ts) $ substituteInTerm s3 e)
                      (Just $ substInTypeScheme senv ts)) b3t eb2

inferTypeOfList :: AltInferenceContext -> [Term] -> Flow s AltInferenceResult
inferTypeOfList cx = inferTypeOfCollection cx Types.list Terms.list "list element"

inferTypeOfLiteral :: AltInferenceContext -> Literal -> Flow s AltInferenceResult
inferTypeOfLiteral _ lit = Flows.pure $ AltInferenceResult (TermLiteral lit) (TypeLiteral $ literalType lit) emptyTypeSubst

inferTypeOfMap :: AltInferenceContext -> M.Map Term Term -> Flow s AltInferenceResult
inferTypeOfMap cx m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        -- TODO: get rid of this special case; it should follow from the generate case
        then Flows.pure $ yield (TermMap M.empty) (Types.map (TypeVariable kvar) (TypeVariable vvar)) emptyTypeSubst
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

inferTypeOfOptional :: AltInferenceContext -> Maybe Term -> Flow s AltInferenceResult
inferTypeOfOptional cx m = inferTypeOfCollection cx Types.optional trmCons "optional element" $ Y.maybe [] (\e -> [e]) m
  where
    trmCons terms = case terms of
      [] -> Terms.optional Nothing
      [term] -> Terms.optional $ Just term

inferTypeOfOptionalCases :: AltInferenceContext -> OptionalCases -> Flow s AltInferenceResult
inferTypeOfOptionalCases cx (OptionalCases n j) = bindVar2 withVars
  where
    withVars domv codv = Flows.bind (inferTwo cx n "nothing case" j "just case") withResults
      where
        dom = TypeVariable domv
        cod = TypeVariable codv
        withResults (nterm, nts, jterm, jts, subst) = mapConstraints cx withSubst [
            TypeConstraint cod nts "optional elimination; nothing case",
            TypeConstraint (Types.function dom cod) jts "optional elimination; just case"]
          where
            withSubst csubst = yield
                (Terms.matchOpt nterm jterm)
                (Types.function (Types.optional dom) cod)
                (composeTypeSubst subst csubst)

inferTypeOfPrimitive :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfPrimitive cx name = case M.lookup name (altInferenceContextPrimitiveTypes cx) of
    Nothing -> Flows.fail $ "No such primitive: " ++ unName name
    -- TODO: check against algo W implementation
    Just scheme -> Flows.map withScheme $ instantiateTypeScheme scheme
  where
    withScheme ts = yield (Terms.primitive name) (typeSchemeType ts) emptyTypeSubst

inferTypeOfProduct :: AltInferenceContext -> [Term] -> Flow s AltInferenceResult
inferTypeOfProduct cx els = Flows.map withResults (inferMany cx $ fmap (\e -> (e, "tuple element")) els)
  where
    withResults (iterms, itypes, isubst) = yield (Terms.product iterms) (Types.product itypes) isubst

inferTypeOfProjection :: AltInferenceContext -> Projection -> Flow s AltInferenceResult
inferTypeOfProjection cx (Projection tname fname) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (expectRecordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ findFieldType fname sfields
          where
            withField ftyp = yield
              (Terms.project tname fname)
              (Types.function (nominalApplication tname svars) ftyp)
              emptyTypeSubst

inferTypeOfRecord :: AltInferenceContext -> Record -> Flow s AltInferenceResult
inferTypeOfRecord cx (Record tname fields) =
    bind2 (requireSchemaType cx tname) (inferMany cx $ fmap (\f -> (fieldTerm f, "field " ++ unName (fieldName f))) fields) withResults
  where
    fnames = fmap fieldName fields
    withResults (TypeScheme svars styp) (iterms, itypes, isubst) = bindConstraints cx [
        TypeConstraint styp ityp "schema type of record"] withSubst
      where
        ityp = TypeRecord $ RowType tname $ L.zipWith FieldType fnames itypes
        withSubst subst = do
--          debugIf "All tests, Inference tests, Nominal terms, Records, Record instances of simply recursive record types, #1" $ ""
--            ++ "\n\tstyp: " ++ showType styp
--            ++ "\n\tityp: " ++ showType ityp

--          yieldDebug cx "All tests, Inference tests, Nominal terms, Records, Record instances of simply recursive record types, #1"
          return $ yield
            (TermRecord $ Record tname $ L.zipWith Field fnames iterms)
            (nominalApplication tname svars)
            (composeTypeSubst isubst subst)

inferTypeOfSet :: AltInferenceContext -> S.Set Term -> Flow s AltInferenceResult
inferTypeOfSet cx = inferTypeOfCollection cx Types.set (Terms.set . S.fromList) "set element" . S.toList

inferTypeOfSum :: AltInferenceContext -> Sum -> Flow s AltInferenceResult
inferTypeOfSum cx (Sum i s term) = bindInferredTerm cx term "sum term" withResult
  where
    withResult (AltInferenceResult iterm ityp isubst) = Flows.map withVars (Flows.sequence $ fmap (varOrTerm ityp) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then Flows.pure $ Left t
          else Right <$> freshName
        withVars vars = yield (TermSum $ Sum i s iterm) (TypeSum $ fmap toType vars) isubst
          where
            toType e = case e of
              Left t -> t
              Right v -> TypeVariable v

inferTypeOfTerm :: AltInferenceContext -> Term -> String -> Flow s AltInferenceResult
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

inferTypeOfTupleProjection :: AltInferenceContext -> TupleProjection -> Flow s AltInferenceResult
inferTypeOfTupleProjection _ (TupleProjection arity idx) = forVars arity withVars
  where
    withVars vars = yield
        (Terms.untuple arity idx)
        (Types.function (Types.product types) cod)
        emptyTypeSubst
      where
        types = TypeVariable <$> vars
        cod = types !! idx

inferTypeOfTypeAbstraction :: AltInferenceContext -> TypeAbstraction -> Flow s AltInferenceResult
inferTypeOfTypeAbstraction cx (TypeAbstraction _ term) = inferTypeOfTerm cx term "type abstraction"

inferTypeOfTypeApplication :: AltInferenceContext -> TypedTerm -> Flow s AltInferenceResult
inferTypeOfTypeApplication cx (TypedTerm term _) = inferTypeOfTerm cx term "type application term"

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: AltInferenceContext -> TypedTerm -> Flow s AltInferenceResult
inferTypeOfTypedTerm cx (TypedTerm term _) = inferTypeOfTerm cx term "typed term"

inferTypeOfUnwrap :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfUnwrap cx tname = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.map withWrappedType (expectWrappedType tname styp)
      where
        withWrappedType wtyp = yield
          (Terms.unwrap tname)
          (Types.function (nominalApplication tname svars) wtyp)
          emptyTypeSubst

inferTypeOfVariable :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfVariable cx name = case M.lookup name (altInferenceContextDataTypes cx) of
    Nothing -> Flows.fail $ "Variable not bound to type: " ++ unName name
    Just scheme -> Flows.map withTypeScheme $ instantiateTypeScheme scheme
  where
    withTypeScheme (TypeScheme vars typ) = AltInferenceResult
      (Terms.typeApplication (TermVariable name) $ fmap TypeVariable vars)
      typ
      emptyTypeSubst

inferTypeOfWrappedTerm :: AltInferenceContext -> WrappedTerm -> Flow s AltInferenceResult
inferTypeOfWrappedTerm cx (WrappedTerm tname term) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term "wrapped term") withResult
  where
    withResult (TypeScheme svars styp) (AltInferenceResult iterm ityp isubst)
        = mapConstraints cx withSubst [
          TypeConstraint styp (TypeWrap $ WrappedType tname ityp) "schema type of wrapper"]
      where
        withSubst subst = yield (Terms.wrap tname iterm) (nominalApplication tname svars) (composeTypeSubst isubst subst)

--------------------------------------------------------------------------------

bindConstraints :: AltInferenceContext -> [TypeConstraint] -> (TypeSubst -> Flow s a) -> Flow s a
bindConstraints cx constraints = Flows.bind (unifyTypeConstraints cx constraints)

bindVar :: (Name -> Flow s a) -> Flow s a
bindVar = Flows.bind freshName

bindVar2 :: (Name -> Name -> Flow s a) -> Flow s a
bindVar2 f = bindVar $ \v1 -> bindVar $ \v2 -> f v1 v2

bindVars :: Int -> ([Name] -> Flow s a) -> Flow s a
bindVars n = Flows.bind $ freshNames n

bindInferredTerm :: AltInferenceContext -> Term -> String -> (AltInferenceResult -> Flow s a) -> Flow s a
bindInferredTerm cx term desc = Flows.bind $ inferTypeOfTerm cx term desc

forInferredTerm :: AltInferenceContext -> Term -> String -> (AltInferenceResult -> a) -> Flow s a
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

mapConstraints :: AltInferenceContext -> (TypeSubst -> a) -> [TypeConstraint] -> Flow s a
mapConstraints cx f constraints = Flows.map f $ unifyTypeConstraints cx constraints

maybeToList :: Maybe a -> [a]
maybeToList mx = case mx of
  Just x -> [x]
  Nothing -> []

nominalApplication :: Name -> [Name] -> Type
nominalApplication tname vars = case vars of
  [] -> TypeVariable tname
  (h:r) -> Types.apply (nominalApplication tname r) (TypeVariable h)

requireSchemaType :: AltInferenceContext -> Name -> Flow s TypeScheme
requireSchemaType cx tname = case M.lookup tname (altInferenceContextSchemaTypes cx) of
    Nothing -> Flows.fail $ "No such schema type: " ++ unName tname
    Just ts -> instantiateTypeScheme $ stripTypeSchemeRecursive ts

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: [(Name, TypeScheme)] -> AltInferenceContext -> AltInferenceContext
extendContext pairs cx = cx {altInferenceContextDataTypes = M.union (M.fromList pairs) $ altInferenceContextDataTypes cx}

yield :: Term -> Type -> TypeSubst -> AltInferenceResult
yield term typ subst = AltInferenceResult (substTypesInTerm subst term) (substInType subst typ) subst

yieldDebug :: AltInferenceContext -> String -> Term -> Type -> TypeSubst -> Flow s AltInferenceResult
yieldDebug cx debugId term typ subst = do

    debugIf debugId $ ""
        ++ "\n\tterm: " ++ showTerm term
        ++ "\n\ttyp: " ++ showType typ
        ++ "\n\tsubst: " ++ showTypeSubst subst
        ++ "\n\trterm: " ++ showTerm rterm
        ++ "\n\trtyp: " ++ showType rtyp

    return $ AltInferenceResult rterm rtyp subst
  where
    rterm = substTypesInTerm subst term
    rtyp = substInType subst typ
