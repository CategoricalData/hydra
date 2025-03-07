module Hydra.Staging.Inference.AltInference where

import Hydra.Variants
import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Errors
import Hydra.Mantle
import Hydra.Flows
import Hydra.Strip
import Hydra.Rewriting
import Hydra.Staging.Annotations
import Hydra.Staging.Rewriting
import Hydra.Staging.Schemas
import Hydra.Lib.Flows as Flows
import qualified Hydra.Tools.Monads as TM
import qualified Hydra.Dsl.Types as Types

import qualified Control.Monad as CM
import qualified Data.Either   as E
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Maybe    as Y


--------------------------------------------------------------------------------
-- Variables

key_vcount = Name "vcount"

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

normalizeVariablesInTypeScheme :: TypeScheme -> TypeScheme
normalizeVariablesInTypeScheme scheme = TypeScheme newVars $ substituteInType subst $ typeSchemeType scheme
  where
    normalVariables = (\n -> Name $ "t" ++ show n) <$> [0..]
    oldVars = typeSchemeVariables scheme
    newVars = L.take (L.length oldVars) normalVariables
    subst =TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)

--------------------------------------------------------------------------------
-- Graphs

showTerm :: Term -> String
showTerm term = case stripTerm term of
  TermApplication app -> "(" ++ (L.intercalate " @ " $ fmap showTerm $ gatherTerms [] app) ++ ")"
    where
      gatherTerms prev (Application lhs rhs) = case stripTerm lhs of
        TermApplication app2 -> gatherTerms (rhs:prev) app2
        t -> t:(L.reverse (rhs:prev))
  TermFunction f -> case f of
    FunctionElimination _ -> show $ stripTerm term
    FunctionLambda (Lambda (Name v) mt body) -> "λ" ++ v ++ (Y.maybe "" (\t -> ":" ++ showType t) mt) ++ "." ++ showTerm body
    FunctionPrimitive (Name name) -> name ++ "!"
  TermLet (Let bindings env) -> "let " ++ (L.intercalate ", " $ fmap showBinding bindings) ++ " in " ++ showTerm env
    where
      showBinding (LetBinding (Name v) term mt) = v ++ "=" ++ showTerm term ++ (Y.maybe "" (\t -> ":" ++ showTypeScheme t) mt)
  TermLiteral lit -> case lit of
    LiteralBinary _ -> "[binary]"
    LiteralBoolean b -> if b then "true" else "false"
    LiteralFloat fv -> case fv of
      FloatValueBigfloat v -> show v
      FloatValueFloat32 v -> show v
      FloatValueFloat64 v -> show v
    LiteralInteger iv -> case iv of
      IntegerValueBigint v -> show v
      IntegerValueInt8 v -> show v
      IntegerValueInt16 v -> show v
      IntegerValueInt32 v -> show v
      IntegerValueInt64 v -> show v
      IntegerValueUint8 v -> show v
      IntegerValueUint16 v -> show v
      IntegerValueUint32 v -> show v
      IntegerValueUint64 v -> show v
    LiteralString s -> show s
  TermTypeAbstraction (TypeAbstraction (Name param) body) -> "Λ" ++ param ++ "." ++ showTerm body
  TermTypeApplication (TypedTerm term typ) -> showTerm term ++ "⟨" ++ showType typ ++ "⟩"
  TermVariable (Name name) -> name
  t -> show t

showType :: Type -> String
showType typ = case stripType typ of
  TypeFunction (FunctionType dom cod) -> "(" ++ showType dom ++ "→" ++ showType cod ++ ")"
  TypeList etyp -> "[" ++ showType etyp ++ "]"
  TypeLiteral lt -> case lt of
    LiteralTypeBinary -> "binary"
    LiteralTypeBoolean -> "boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeBigfloat -> "bigfloat"
      FloatTypeFloat32 -> "float32"
      FloatTypeFloat64 -> "float64"
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> "bigint"
      IntegerTypeInt8 -> "int8"
      IntegerTypeInt16 -> "int16"
      IntegerTypeInt32 -> "int32"
      IntegerTypeInt64 -> "int64"
      IntegerTypeUint8 -> "uint8"
      IntegerTypeUint16 -> "uint16"
      IntegerTypeUint32 -> "uint32"
      IntegerTypeUint64 -> "uint64"
    LiteralTypeString -> "string"
  TypeMap (MapType keyTyp valTyp) -> "map<" ++ showType keyTyp ++ "," ++ showType valTyp ++ ">"
  TypeProduct types -> "(" ++ (L.intercalate "," (fmap showType types)) ++ ")"
  TypeVariable (Name name) -> name
  t -> show t

showTypeScheme :: TypeScheme -> String
showTypeScheme (TypeScheme vars body) = fa ++ showType body
  where
    fa = if L.null vars then "" else "∀[" ++ (L.intercalate "," (fmap (\(Name name) -> name) vars)) ++ "]."

showConstraint :: TypeConstraint -> String
showConstraint (TypeConstraint ltyp rtyp _) = showType ltyp ++ "≡" ++ showType rtyp

showTypeSubst :: TypeSubst -> String
showTypeSubst (TypeSubst subst) = "{" ++ (L.intercalate "," (fmap (\(Name name, typ) -> name ++ "↦" ++ showType typ) (M.toList subst))) ++ "}"

--------------------------------------------------------------------------------
-- Unification

-- TODO: eliminate lambda types before unification
-- TODO: reduce application types before unification

data UnificationError
  = UnificationErrorCannotUnify Type Type (Maybe String)
  | UnificationErrorOccursCheckFailed Name Type (Maybe String)
  deriving (Eq, Ord, Show)

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
unifyTypeConstraints :: [TypeConstraint] -> Either UnificationError TypeSubst
unifyTypeConstraints [] = Right emptyTypeSubst
unifyTypeConstraints ((TypeConstraint left right comment):rest) = case sleft of
    TypeVariable name -> case sright of
        TypeVariable name2 -> if name == name2
          then unifyTypeConstraints rest
          else bind name sright
        _ -> tryBinding name sright
    _ -> case sright of
      TypeVariable name -> tryBinding name sleft
      _ -> do
        constraints2 <- joinTypes sleft sright comment
        unifyTypeConstraints $ constraints2 ++ rest
  where
    sleft = stripType left
    sright = stripType right
    -- TODO: this occurrence check is expensive; consider delaying it until the time of substitution
    tryBinding v t = if variableOccursInType v t
      then Left $ UnificationErrorOccursCheckFailed v t comment
      else bind v t
    bind v t = composeTypeSubst subst <$> unifyTypeConstraints (substituteInConstraints subst rest)
      where
        subst = singletonTypeSubst v t

unifyTypes :: [Type] -> [Type] -> Maybe String -> Either UnificationError TypeSubst
unifyTypes l r comment = unifyTypeConstraints $ L.zipWith toConstraint l r
  where
    toConstraint l r = TypeConstraint l r comment

joinTypes :: Type -> Type -> Maybe String -> Either UnificationError [TypeConstraint]
joinTypes left right comment = case sleft of
    TypeFunction (FunctionType domleft codleft) -> case sright of
      TypeFunction (FunctionType domright codright) -> Right [
        joinOne domleft domright,
        joinOne codleft codright]
    TypeList eleft -> case sright of
      TypeList eright -> Right [joinOne eleft eright]
      _ -> cannotUnify
    TypeLiteral ltleft -> assertEqual
    TypeMap (MapType kleft vleft) -> case sright of
      TypeMap (MapType kright vright) -> Right [
        joinOne kleft kright,
        joinOne vleft vright]
      _ -> cannotUnify
    TypeOptional eleft -> case sright of
      TypeOptional eright -> Right [joinOne eleft eright]
      _ -> cannotUnify
    TypeProduct lefts -> case sright of
      TypeProduct rights -> joinList lefts rights
      _ -> cannotUnify
    TypeRecord rtleft -> case sright of
      TypeRecord rtright -> joinRowTypes rtleft rtright
      _ -> cannotUnify
    TypeSet eleft -> case sright of
      TypeSet eright -> Right [joinOne eleft eright]
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
        else Right [joinOne eleft eright]
      _ -> cannotUnify
    -- TypeAnnotated, TypeApplication, TypeLambda, TypeVariable should not appear here
    _ -> cannotUnify
  where
    sleft = stripType left
    sright = stripType right
    joinOne l r = TypeConstraint l r Nothing
    cannotUnify = Left $ UnificationErrorCannotUnify sleft sright comment
    assertEqual = if sleft == sright
      then Right []
      else cannotUnify
    joinList lefts rights = if L.length lefts == L.length rights
      then Right $ L.zipWith joinOne lefts rights
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

data TypeSubst = TypeSubst { unTypeSubst :: M.Map Name Type } deriving (Eq, Ord)
data TermSubst = TermSubst { unTermSubst :: M.Map Name Term } deriving (Eq, Ord)

instance Show TypeSubst where
  show (TypeSubst subst) = "{" ++ L.intercalate ", " (fmap (\((Name k), v) -> k ++ ": " ++ showType v) $ M.toList subst) ++ "}"

composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst (TypeSubst firstMap) second@(TypeSubst secondMap) = TypeSubst $
  M.union (substituteInType second <$> firstMap) secondMap

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

substituteInType :: TypeSubst -> Type -> Type
substituteInType subst = rewriteType rewrite
  where
    rewrite recurse typ = case recurse typ of
      TypeVariable name -> case M.lookup name (unTypeSubst subst) of
        Just styp -> styp
        Nothing -> typ
      t -> t

substituteInTypeScheme :: TypeSubst -> TypeScheme -> TypeScheme
substituteInTypeScheme subst (TypeScheme vars typ) = TypeScheme vars $ substituteInType subst2 typ
  where
    subst2 = TypeSubst $ M.filterWithKey (\k _ -> not $ k `elem` vars) $ unTypeSubst subst

substituteInTypeSchemeLegacy :: TypeSubst -> TypeScheme -> TypeScheme
substituteInTypeSchemeLegacy subst (TypeScheme vars typ) = TypeScheme vars $ substituteInType subst typ

substituteInConstraint :: TypeSubst -> TypeConstraint -> TypeConstraint
substituteInConstraint subst (TypeConstraint t1 t2 ctx) = TypeConstraint (substituteInType subst t1) (substituteInType subst t2) ctx

substituteInConstraints :: TypeSubst -> [TypeConstraint] -> [TypeConstraint]
substituteInConstraints subst = fmap (substituteInConstraint subst)

substituteTypesInTerm :: TypeSubst -> Term -> Term
substituteTypesInTerm subst = rewriteTerm rewrite
  where
    rewrite recurse term = case term of
      -- TODO: injections and case statements need a domain field as well, similar to lambdas
      TermFunction (FunctionLambda (Lambda v mt body)) -> recurse $ TermFunction $ FunctionLambda $ Lambda v (fmap (substituteInType subst) mt) body
      TermLet (Let bindings env) -> recurse $ TermLet $ Let (fmap rewriteBinding bindings) env
        where
          rewriteBinding (LetBinding v e mt) = LetBinding v e $ fmap (substituteInTypeSchemeLegacy subst) mt
      TermTypeAbstraction (TypeAbstraction param body) -> TermTypeAbstraction $ TypeAbstraction param $ substituteTypesInTerm subst2 body
        where
          subst2 = TypeSubst $ M.delete param $ unTypeSubst subst
      TermTypeApplication (TypedTerm trm typ) -> recurse $ TermTypeApplication $ TypedTerm trm $ substituteInType subst typ
      _ -> recurse term

--------------------------------------------------------------------------------
-- Inference

data AltInferenceContext
  = AltInferenceContext {
    altInferenceContextTypes :: M.Map Name TypeScheme}
--    altInferenceContextPrimitiveTypes :: M.Map Name TypeScheme,
--    altInferenceContextSchemaTypes :: M.Map Name TypeScheme,
--    altInferenceContextVariableTypes :: M.Map Name TypeScheme}
  deriving (Eq, Ord, Show)

data AltInferenceResult
  = AltInferenceResult {
    altInferenceResultTerm :: Term,
    altInferenceResultTypeScheme :: TypeScheme, -- TODO: no need for type schemes here; we can just pass (STLC) types up
    altInferenceResultConstraints :: [TypeConstraint], -- TODO: no need to pass constraints up; we pass substitutions
    altInferenceResultSubst :: TypeSubst}
  deriving (Eq, Ord)
instance Show AltInferenceResult where
  show (AltInferenceResult term scheme constraints subst) = "{"
    ++ "term=" ++ showTerm term ++ ", "
    ++ "type= " ++ showTypeScheme scheme ++ ", "
    ++ "constraints= [" ++ (L.intercalate ", " $ fmap showConstraint constraints) ++ "], "
    ++ "subst= " ++ showTypeSubst subst ++ "}"

dummyTerm :: Term
dummyTerm = TermLiteral $ LiteralString "dummy"

-- | Warning: this is an expensive operation (linear with respect to the size of the graph and schema graph)
generalize :: AltInferenceContext -> Type -> TypeScheme
generalize cx typ = TypeScheme vars typ
  where
    vars = L.nub $ L.filter isUnbound $ S.toList $ freeVariablesInType typ
    isUnbound v = not $ S.member v freeVars
    freeVars = L.foldl S.union S.empty $ fmap freeVariablesInTypeScheme $ M.elems $ altInferenceContextTypes cx

inferTypeOf :: AltInferenceContext -> Term -> Flow s TypeScheme
inferTypeOf cx term = bindInferredTerm cx term unifyAndSubst
  where
    unifyAndSubst result = Flows.bind (TM.fromEither $ unifyTypeConstraints $ altInferenceResultConstraints result) doSubst
      where
        subst1 = altInferenceResultSubst result
        -- TODO: get rid of subst2 along with passed constraints
        doSubst subst2 = instantiateTypeSchemeAndNormalize $ substituteInTypeSchemeLegacy subst $ altInferenceResultTypeScheme result
          where
            subst = composeTypeSubst subst1 subst2

inferTypeOfAnnotatedTerm :: AltInferenceContext -> AnnotatedTerm -> Flow s AltInferenceResult
inferTypeOfAnnotatedTerm cx (AnnotatedTerm term _) = inferTypeOfTerm cx term

inferTypeOfApplication :: AltInferenceContext -> Application -> Flow s AltInferenceResult
inferTypeOfApplication cx (Application lterm rterm) = bindVar2 withVars
  where
    withVars dom cod = bind2 (inferTypeOfTerm cx lterm) (inferTypeOfTerm cx rterm) withResults
      where
        withResults lresult rresult = mapConstraints withConstraints [
            TypeConstraint (Types.function (TypeVariable dom) (TypeVariable cod)) ltyp $ Just "application; lhs",
            TypeConstraint (TypeVariable dom) rtyp $ Just "application; rhs"]
          where
            withConstraints subst = AltInferenceResult
                (TermApplication $ Application (altInferenceResultTerm lresult) (altInferenceResultTerm rresult))
                (TypeScheme tvars $ TypeVariable cod)
                ([TypeConstraint (Types.function (TypeVariable dom) (TypeVariable cod)) ltyp $ Just "application; lhs",
                  TypeConstraint (TypeVariable dom) rtyp $ Just "application; rhs"]
                 ++ altInferenceResultConstraints lresult ++ altInferenceResultConstraints rresult)
                (composeTypeSubstList [subst, altInferenceResultSubst lresult, altInferenceResultSubst rresult])
            ltyp = typeSchemeType $ altInferenceResultTypeScheme lresult
            rtyp = typeSchemeType $ altInferenceResultTypeScheme rresult
            tvars = typeSchemeVariables (altInferenceResultTypeScheme lresult) ++ typeSchemeVariables (altInferenceResultTypeScheme rresult)

inferTypeOfCaseStatement :: AltInferenceContext -> CaseStatement -> Flow s AltInferenceResult
inferTypeOfCaseStatement cx (CaseStatement tname dflt cases) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (expectUnionType tname styp) withFields
      where
        withFields sfields = bind2 (traverse (inferTypeOfTerm cx) dflt) (Flows.sequence $ fmap (inferTypeOfTerm cx) $ fmap fieldTerm cases) withResults
          where
            withResults mr rcases = bindVar withCod
              where
                withCod codv = mapConstraints withConstraints (dfltConstraints ++ caseConstraints)
                  where
                    cod = TypeVariable codv
                    dfltConstraints = maybeToList $ fmap (\r -> TypeConstraint cod (typeSchemeType $ altInferenceResultTypeScheme r) $ Just "match default") mr
                    caseConstraints = Y.catMaybes $ fmap (\(FieldType fname ftyp) -> fmap (\r -> toConstraint ftyp r) $ M.lookup fname caseMap) sfields
                      where
                        caseMap = M.fromList $ L.zipWith (\f r -> (fieldName f, r)) cases rcases
                        toConstraint ftyp r = TypeConstraint (Types.function ftyp cod) (typeSchemeType $ altInferenceResultTypeScheme r) $ Just "case type"
                    vars =
                      svars
                      ++ (L.concat $ fmap (typeSchemeVariables . altInferenceResultTypeScheme) rcases)
                      ++ Y.fromMaybe [] (fmap (typeSchemeVariables . altInferenceResultTypeScheme) mr)
                    cons =
                      dfltConstraints
                      ++ caseConstraints
                      ++ (L.concat $ fmap altInferenceResultConstraints rcases)
                      ++ (Y.maybe [] altInferenceResultConstraints mr)
                    withConstraints subst = AltInferenceResult
                        dummyTerm
                        (TypeScheme vars $ TypeFunction $ FunctionType styp cod)
                        cons
                        (composeTypeSubstList $ [subst] ++ (maybeToList $ fmap altInferenceResultSubst mr) ++ (fmap altInferenceResultSubst rcases))

inferTypeOfCollection :: AltInferenceContext -> (Type -> Type) -> String -> [Term] -> Flow s AltInferenceResult
inferTypeOfCollection cx cons desc els = bindVar withVar
  where
    withVar var = forInferredTerms cx els fromResults
      where
        fromResults results = AltInferenceResult
            dummyTerm
            (TypeScheme vars $ cons $ TypeVariable var)
            constraints
            (composeTypeSubstList $ fmap altInferenceResultSubst results)
          where
            uctx = Just desc
            constraints = cinner ++ couter
            cinner = L.concat (altInferenceResultConstraints <$> results)
            couter = fmap (\t -> TypeConstraint (TypeVariable var) t uctx) types
            types = typeSchemeType . altInferenceResultTypeScheme <$> results
            vars = [var] ++ (L.concat (typeSchemeVariables . altInferenceResultTypeScheme <$> results))

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
    withVars a b = Flows.bind (inferTypeOfTerm cx fun) withResult
      where
        withResult (AltInferenceResult _ (TypeScheme funVars funType) constraints subst1) = mapConstraints withSubst [funCons]
          where
            withSubst subst2 = AltInferenceResult
              dummyTerm
              (TypeScheme (a:b:funVars) elimType)
              (funCons:constraints)
              (composeTypeSubst subst1 subst2)
            funCons = TypeConstraint expectedType funType $ Just "fold function"
            aT = TypeVariable a
            bT = TypeVariable b
            expectedType = Types.functionN [bT, aT, bT]
            elimType = Types.functionN [bT, Types.list aT, bT]

inferTypeOfFunction :: AltInferenceContext -> Function -> Flow s AltInferenceResult
inferTypeOfFunction cx f = case f of
  FunctionElimination elm -> inferTypeOfElimination cx elm
  FunctionLambda l -> inferTypeOfLambda cx l
  FunctionPrimitive name -> inferTypeOfPrimitive cx name

inferTypeOfInjection :: AltInferenceContext -> Injection -> Flow s AltInferenceResult
inferTypeOfInjection cx (Injection tname (Field fname term)) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term) withResults
  where
    withResults (TypeScheme svars styp) (AltInferenceResult _ (TypeScheme ivars ityp) icons isubst) =
        Flows.bind (expectUnionType tname styp) withFields
      where
        withFields sfields = Flows.bind (findFieldType fname sfields) withField
          where
            withField ftyp = mapConstraints withSubst [scons]
              where
                scons = TypeConstraint ftyp ityp $ Just "schema type of injected field"
                withSubst subst = AltInferenceResult
                  dummyTerm
                  (TypeScheme (svars ++ ivars) styp)
                  ([scons] ++ icons)
                  (composeTypeSubst subst isubst)

inferTypeOfLambda :: AltInferenceContext -> Lambda -> Flow s AltInferenceResult
inferTypeOfLambda cx (Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = Flows.map withResult (inferTypeOfTerm cx2 body)
      where
        cx2 = extendContext [(var, Types.mono $ TypeVariable vdom)] cx
        withResult (AltInferenceResult ebody (TypeScheme vbody tbody) cbody sbody)
          = AltInferenceResult
            (TermFunction $ FunctionLambda $ Lambda var (Just $ TypeVariable vdom) ebody)
            (TypeScheme (vdom:vbody) $ Types.function (TypeVariable vdom) tbody)
            cbody
            sbody






inferTypeOfLet :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfLet = inferTypeOfLetNew

-- TODO: this rule contains some expensive operations. Various optimizations may be possible, such as saving substitutions until the end (outside of the rule itself).
inferTypeOfLetNew :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfLetNew cx (Let b0 env0) = bindVars (L.length b0) withVars
  where
    bnames = fmap letBindingName b0
    eb0 = fmap letBindingTerm b0
    withVars bvars = Flows.bind (Flows.sequence $ fmap (inferTypeOfTerm cx2) eb0) withInferredBindings
      where
        cx2 = extendContext (L.zip bnames $ fmap Types.mono btypes) cx
        btypes = fmap TypeVariable bvars
        withInferredBindings results = Flows.bind
            (TM.fromEither $ unifyTypes (fmap (substituteInType s1) btypes) tb $ Just "let temporary type bindings") withSubst
          where
            eb1 = fmap altInferenceResultTerm results
            tb = fmap (typeSchemeType . altInferenceResultTypeScheme) results
            s1 = composeTypeSubstList $ fmap altInferenceResultSubst results
            withSubst sb = Flows.bind (inferTypeOfTerm cx3 env0) withEnv
              where
                cx3 = extendContext (L.zip bnames $ fmap (generalize cx . substituteInType sb) tb) cx2
                withEnv tmp@(AltInferenceResult env1 tenv _ senv) = do
--                            if (fullyStripTerm env0) == (TermApplication $ Application (TermVariable $ Name "f") (TermLiteral $ LiteralInteger $ IntegerValueInt32 0))
                    if (fullyStripTerm env0) == (TermVariable $ Name "f")
                      then Flows.fail $ "bnames: " ++ L.intercalate ", " (fmap unName bnames)
                        ++ "\nbvars: " ++ L.intercalate ", " (fmap unName bvars)
                        ++ "\nenv result: " ++ show tmp
                        ++ "\nbinding results: " ++ L.concat (fmap (\r -> "\n\t" ++ show r) results)
                        ++ "\noverall result: " ++ show (AltInferenceResult let1 tenv [] s2)
                      else Flows.pure ()
                    return $ AltInferenceResult let1 tenv [] s2
                  where
                    s2 = composeTypeSubstList [senv, sb, s1]
                    eb2 = fmap (substituteTypesInTerm $ composeTypeSubst senv sb) eb1
                    b3t = L.zip bnames $ fmap (generalize cx . substituteInType s2) tb
                    s3 = TermSubst $ M.fromList $ fmap (\(x, ts) -> (x, (typeApplication (TermVariable x) $ fmap TypeVariable $ typeSchemeVariables ts))) b3t
                    b3 = L.zipWith (\(x, ts) e -> LetBinding x
                      (substituteTypesInTerm (composeTypeSubst sb senv) $ typeAbstraction (typeSchemeVariables ts) $ substituteInTerm s3 e)
                      (Just $ substituteInTypeScheme senv ts)) b3t eb2
                    let1 = TermLet $ Let b3 env1

inferTypeOfLetOld :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfLetOld cx (Let bindings env) = bindVars (L.length bindings) withVars
  where
    bindingNames = letBindingName <$> bindings
    bindingTerms = letBindingTerm <$> bindings
    withVars bvars = Flows.bind (Flows.sequence $ fmap (inferTypeOfTerm cx2) bindingTerms) withInferredBindings
      where
        cx2 = extendContext (L.zip bindingNames $ fmap (TypeScheme []) btypes) cx
        btypes = fmap TypeVariable bvars
        withInferredBindings rbindings = Flows.map withInferredEnv (inferTypeOfTerm cx3 env)
          where
            cx3 = extendContext (L.zip bindingNames $ fmap altInferenceResultTypeScheme rbindings) cx2
            withInferredEnv renv = AltInferenceResult dummyTerm (TypeScheme vars typ) constraints tmpTypeSubst
              where
                typ = typeSchemeType $ altInferenceResultTypeScheme renv
                vars =
                  bvars
                  ++ (L.concat $ typeSchemeVariables . altInferenceResultTypeScheme <$> rbindings)
                  ++ (typeSchemeVariables $ altInferenceResultTypeScheme renv)
                constraints =
                  (L.concat $ altInferenceResultConstraints <$> rbindings)
                  ++ (altInferenceResultConstraints renv)
                  ++ (L.zipWith
                    (\t1 t2 -> TypeConstraint t1 t2 $ Just "temp variable for let binding")
                    btypes
                    (typeSchemeType . altInferenceResultTypeScheme <$> rbindings))

-- TODO: propagate rawValueVars and envVars into the final result, possibly after substitution
-- TODO: recursive and mutually recursive let
inferTypeOfLetOlder :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfLetOlder cx (Let bindings env) = if L.null bindings
    then Flows.map forEmptyBindings (inferTypeOfTerm cx env)
    else if L.length bindings >= 2
    then inferTypeOfTerm cx $ TermLet $ Let [L.head bindings] $ TermLet $ Let (L.tail bindings) env
    else forSingleBinding $ L.head bindings
  where
    forEmptyBindings r = r
    forSingleBinding (LetBinding key value _) = bindVar withVar
      where
        -- Create a temporary type variable for the binding
        withVar var = bindInferredTerm cx2 value withValueType
          where
            cx2 = extendContext [(key, Types.mono $ TypeVariable var)] cx
            -- Unify and substitute over the value constraints
            -- TODO: save the substitution and pass it along, instead of the original set of constraints
            withValueType (AltInferenceResult _ rawValueScheme valueConstraints _) = Flows.bind (TM.fromEither $ unifyTypeConstraints kvConstraints) afterUnification
              where
                rawValueVars = typeSchemeVariables rawValueScheme
                kvConstraints = keyConstraint:valueConstraints
                keyConstraint = TypeConstraint (TypeVariable var) (typeSchemeType rawValueScheme) $ Just "let binding"
                -- Now update the type binding to use the inferred type
                afterUnification subst = Flows.map withEnvType (inferTypeOfTerm cx3 env)
                  where
                    cx3 = extendContext [(key, valueScheme)] cx2
                    valueScheme = substituteInTypeSchemeLegacy subst rawValueScheme
                    withEnvType (AltInferenceResult _ envScheme envConstraints _) = AltInferenceResult dummyTerm envScheme constraints emptyTypeSubst
                      where
                        constraints = kvConstraints ++ envConstraints
                        envVars = typeSchemeVariables envScheme








inferTypeOfList :: AltInferenceContext -> [Term] -> Flow s AltInferenceResult
inferTypeOfList cx = inferTypeOfCollection cx Types.list "list element"

inferTypeOfLiteral :: AltInferenceContext -> Literal -> Flow s AltInferenceResult
inferTypeOfLiteral _ lit = Flows.pure $ AltInferenceResult (TermLiteral lit) (Types.mono $ TypeLiteral $ literalType lit) [] emptyTypeSubst

inferTypeOfMap :: AltInferenceContext -> M.Map Term Term -> Flow s AltInferenceResult
inferTypeOfMap cx m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        -- TODO: get rid of this special case; it should follow from the generate case
        then Flows.pure $ AltInferenceResult dummyTerm (TypeScheme [kvar, vvar] $ Types.map (TypeVariable kvar) (TypeVariable vvar)) [] emptyTypeSubst
        else Flows.map withResults (Flows.sequence $ fmap fromPair $ M.toList m)
      where
        fromPair (k, v) = bindInferredTerm2 cx k v withPairResults
          where
            withPairResults
                (AltInferenceResult _ (TypeScheme kvars kt) kconstraints ksubst)
                (AltInferenceResult _ (TypeScheme vvars vt) vconstraints vsubst) = mapConstraints withSubst cons
              where
                cons = [
                  TypeConstraint (TypeVariable kvar) kt $ Just "map key",
                  TypeConstraint (TypeVariable vvar) vt $ Just "map value"]
                withSubst subst = ((kvars ++ vvars, cons ++ kconstraints ++ vconstraints), subst)
        withResults pairs = AltInferenceResult
          dummyTerm
          (TypeScheme (L.concat $ fmap (fst . fst)pairs) $ Types.map (TypeVariable kvar) (TypeVariable vvar))
          (L.concat $ fmap (snd . fst) pairs)
          (composeTypeSubstList $ fmap snd pairs)

inferTypeOfOptional :: AltInferenceContext -> Maybe Term -> Flow s AltInferenceResult
inferTypeOfOptional cx m = inferTypeOfCollection cx Types.optional "optional element" $ Y.maybe [] (\e -> [e]) m

inferTypeOfOptionalCases :: AltInferenceContext -> OptionalCases -> Flow s AltInferenceResult
inferTypeOfOptionalCases cx (OptionalCases n j) = bindVar2 withVars
  where
    withVars domv codv = bindInferredTerm2 cx n j withResults
      where
        dom = TypeVariable domv
        cod = TypeVariable codv
        withResults nresult jresult = mapConstraints withSubst cons
          where
            ntyp = typeSchemeType $ altInferenceResultTypeScheme nresult
            jtyp = typeSchemeType $ altInferenceResultTypeScheme jresult
            cons = [
              TypeConstraint cod ntyp $ Just "optional elimination; nothing case",
              TypeConstraint (Types.function dom cod) jtyp $ Just "optional elimination; just case"]
            withSubst subst = AltInferenceResult
                dummyTerm
                (TypeScheme tvars $ Types.function (Types.optional dom) cod)
                constraints
                (composeTypeSubstList [subst, altInferenceResultSubst nresult, altInferenceResultSubst jresult])
              where
                tvars = [domv, codv]
                  ++ typeSchemeVariables (altInferenceResultTypeScheme nresult)
                  ++ typeSchemeVariables (altInferenceResultTypeScheme jresult)
                constraints = cons ++ altInferenceResultConstraints nresult ++ altInferenceResultConstraints jresult

inferTypeOfPrimitive :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfPrimitive cx name = case M.lookup name (altInferenceContextTypes cx) of
  Just scheme -> Flows.map (\ts -> AltInferenceResult (TermFunction $ FunctionPrimitive name) ts [] emptyTypeSubst) $ instantiateTypeScheme scheme
  Nothing -> Flows.fail $ "No such primitive: " ++ unName name

inferTypeOfProduct :: AltInferenceContext -> [Term] -> Flow s AltInferenceResult
inferTypeOfProduct cx els = if L.null els
    -- TODO: get rid of this special case; it should follow from the generate case
    then Flows.pure $ AltInferenceResult dummyTerm (Types.mono $ Types.product []) [] emptyTypeSubst
    else forInferredTerms cx els fromResults
  where
    fromResults results = AltInferenceResult dummyTerm (TypeScheme tvars $ TypeProduct tbodies) constraints subst
      where
        tvars = L.concat $ typeSchemeVariables . altInferenceResultTypeScheme <$> results
        tbodies = fmap (typeSchemeType . altInferenceResultTypeScheme) results
        constraints = L.concat $ fmap altInferenceResultConstraints results
        subst = composeTypeSubstList $ fmap altInferenceResultSubst results

inferTypeOfProjection :: AltInferenceContext -> Projection -> Flow s AltInferenceResult
inferTypeOfProjection cx (Projection tname fname) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (expectRecordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ findFieldType fname sfields
          where
            withField ftyp = AltInferenceResult dummyTerm (TypeScheme svars $ Types.function styp ftyp) [] emptyTypeSubst

inferTypeOfRecord :: AltInferenceContext -> Record -> Flow s AltInferenceResult
inferTypeOfRecord cx (Record tname fields) = bind2
    (requireSchemaType cx tname)
    (Flows.sequence $ fmap (inferTypeOfTerm cx) $ fmap fieldTerm fields)
    withResults
  where
    withResults (TypeScheme svars styp) results = mapConstraints withSubst [scons]
      where
        scons = TypeConstraint styp ityp $ Just "schema type of record"
        ityp = TypeRecord $ RowType tname
          $ L.zipWith (\n r -> FieldType n $ typeSchemeType $ altInferenceResultTypeScheme r) (fmap fieldName fields) results
        withSubst subst = AltInferenceResult
            dummyTerm
            (TypeScheme vars styp)
            (scons:(L.concat $ fmap altInferenceResultConstraints results))
            (composeTypeSubstList (subst:(fmap altInferenceResultSubst results)))
          where
            vars = svars ++ (L.concat $ fmap (typeSchemeVariables . altInferenceResultTypeScheme) results)

inferTypeOfSet :: AltInferenceContext -> S.Set Term -> Flow s AltInferenceResult
inferTypeOfSet cx = inferTypeOfCollection cx Types.set "set element" . S.toList

inferTypeOfSum :: AltInferenceContext -> Sum -> Flow s AltInferenceResult
inferTypeOfSum cx (Sum i s term) = bindInferredTerm cx term withResult
  where
    withResult (AltInferenceResult _ (TypeScheme bvars typ) icons isubst) = Flows.map withVars (Flows.sequence $ fmap (varOrTerm typ) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then Flows.pure $ Left t
          else Right <$> freshName
        withVars vars = AltInferenceResult dummyTerm (TypeScheme (bvars ++ E.rights vars) $ TypeSum (toType <$> vars)) icons isubst
          where
            toType e = case e of
              Left t -> t
              Right v -> TypeVariable v

inferTypeOfTerm :: AltInferenceContext -> Term -> Flow s AltInferenceResult
inferTypeOfTerm cx term = case term of
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
  TermTyped t -> inferTypeOfTypedTerm cx t
  TermUnion i -> inferTypeOfInjection cx i
  TermVariable name -> inferTypeOfVariable cx name
  TermWrap w -> inferTypeOfWrappedTerm cx w

inferTypeOfTupleProjection :: AltInferenceContext -> TupleProjection -> Flow s AltInferenceResult
inferTypeOfTupleProjection _ (TupleProjection arity idx) = forVars arity withVars
  where
    withVars vars = AltInferenceResult dummyTerm (TypeScheme vars $ Types.function (Types.product types) cod) [] emptyTypeSubst
      where
        types = TypeVariable <$> vars
        cod = types !! idx

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: AltInferenceContext -> TypedTerm -> Flow s AltInferenceResult
inferTypeOfTypedTerm cx (TypedTerm term _) = inferTypeOfTerm cx term

inferTypeOfUnwrap :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfUnwrap cx tname = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.map withWrappedType (expectWrappedType tname styp)
      where
        withWrappedType wtyp = AltInferenceResult dummyTerm (TypeScheme svars $ Types.function styp wtyp) [] emptyTypeSubst

inferTypeOfVariable :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfVariable cx name = case M.lookup name (altInferenceContextTypes cx) of
  Just scheme -> Flows.map (\ts -> AltInferenceResult (TermVariable name) ts [] emptyTypeSubst) $ instantiateTypeScheme scheme
  Nothing -> Flows.fail $ "Variable not bound to type: " ++ unName name

inferTypeOfWrappedTerm :: AltInferenceContext -> WrappedTerm -> Flow s AltInferenceResult
inferTypeOfWrappedTerm cx (WrappedTerm tname term) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term) withResult
  where
    withResult (TypeScheme svars styp) (AltInferenceResult _ (TypeScheme ivars ityp) icons isubst) = mapConstraints withSubst [scons]
      where
        scons = TypeConstraint styp (TypeWrap $ WrappedType tname ityp) $ Just "schema type of wrapper"
        withSubst subst = AltInferenceResult
          dummyTerm
          (TypeScheme (svars ++ ivars) styp)
          ([scons] ++ icons)
          (composeTypeSubst subst isubst)

--------------------------------------------------------------------------------

bindVar :: (Name -> Flow s a) -> Flow s a
bindVar = Flows.bind freshName

bindVar2 :: (Name -> Name -> Flow s a) -> Flow s a
bindVar2 f = bindVar $ \v1 -> bindVar $ \v2 -> f v1 v2

bindVars :: Int -> ([Name] -> Flow s a) -> Flow s a
bindVars n = Flows.bind $ freshNames n

bindInferredTerm :: AltInferenceContext -> Term -> (AltInferenceResult -> Flow s a) -> Flow s a
bindInferredTerm cx term = Flows.bind $ inferTypeOfTerm cx term

bindInferredTerm2 :: AltInferenceContext -> Term -> Term -> (AltInferenceResult -> AltInferenceResult -> Flow s a) -> Flow s a
bindInferredTerm2 cx t1 t2 f = bindInferredTerm cx t1 $ \r1 -> bindInferredTerm cx t2 $ f r1

forInferredTerm :: AltInferenceContext -> Term -> (AltInferenceResult -> a) -> Flow s a
forInferredTerm cx term f = Flows.map f $ inferTypeOfTerm cx term

forInferredTerm2 :: AltInferenceContext -> Term -> Term -> (AltInferenceResult -> AltInferenceResult -> a) -> Flow s a
forInferredTerm2 cx t1 t2 f = map2 (inferTypeOfTerm cx t1) (inferTypeOfTerm cx t2) f

forInferredTerms :: AltInferenceContext -> [Term] -> ([AltInferenceResult] -> a) -> Flow s a
forInferredTerms cx terms f = Flows.map f $ Flows.sequence $ inferTypeOfTerm cx <$> terms

forVar :: (Name -> a) -> Flow s a
forVar f = Flows.map f freshName

forVars :: Int -> ([Name] -> a) -> Flow s a
forVars n f = Flows.map f $ freshNames n

instantiateTypeScheme :: TypeScheme -> Flow s TypeScheme
instantiateTypeScheme scheme = Flows.map doSubst (freshNames $ L.length oldVars)
    where
      doSubst newVars = TypeScheme newVars $ substituteInType subst $ typeSchemeType scheme
        where
          subst = TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)
      oldVars = L.intersect (L.nub $ typeSchemeVariables scheme) (freeVariablesInSimpleType $ typeSchemeType scheme)

instantiateTypeSchemeAndNormalize :: TypeScheme -> Flow s TypeScheme
instantiateTypeSchemeAndNormalize scheme = Flows.map normalizeVariablesInTypeScheme (instantiateTypeScheme scheme)

-- Note: does not account for "lambda types"; here we use type schemes instead.
freeVariablesInSimpleType :: Type -> [Name]
freeVariablesInSimpleType = L.nub . foldOverType TraversalOrderPre fold []
  where
    fold rest typ = case typ of
      TypeVariable name -> name : rest
      _ -> rest

mapConstraints :: (TypeSubst -> a) -> [TypeConstraint] -> Flow s a
mapConstraints f constraints = Flows.map f $ TM.fromEither $ unifyTypeConstraints constraints

maybeToList :: Maybe a -> [a]
maybeToList mx = case mx of
  Just x -> [x]
  Nothing -> []

requireSchemaType :: AltInferenceContext -> Name -> Flow s TypeScheme
requireSchemaType cx tname = case M.lookup tname (altInferenceContextTypes cx) of
    Nothing -> Flows.fail $ "No such schema type: " ++ unName tname
    Just ts -> instantiateTypeScheme $ stripTypeSchemeRecursive ts

typeAbstraction :: [Name] -> Term -> Term
typeAbstraction vars body = L.foldl (\b v -> TermTypeAbstraction $ TypeAbstraction v b) body vars

typeApplication :: Term -> [Type] -> Term
typeApplication term types = L.foldl (\t ty -> TermTypeApplication $ TypedTerm t ty) term types

-- | Add (term variable, type scheme) pairs to the typing environment
extendContext :: [(Name, TypeScheme)] -> AltInferenceContext -> AltInferenceContext
extendContext pairs cx = cx {altInferenceContextTypes = M.union (M.fromList pairs) $ altInferenceContextTypes cx}
