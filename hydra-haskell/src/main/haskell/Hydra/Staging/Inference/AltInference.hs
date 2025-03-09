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

key_vcount = Name "vcount"
key_debugId = Name "debugId"

debug :: String -> String -> Flow s ()
debug debugId message = do
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

normalizeVariablesInTypeScheme :: TypeScheme -> TypeScheme
normalizeVariablesInTypeScheme scheme = TypeScheme newVars $ substInType subst $ typeSchemeType scheme
  where
    normalVariables = (\n -> Name $ "t" ++ show n) <$> [0..]
    oldVars = typeSchemeVariables scheme
    newVars = L.take (L.length oldVars) normalVariables
    subst =TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)

--------------------------------------------------------------------------------
-- Printing

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
  TermList els -> "[" ++ (L.intercalate ", " $ fmap showTerm els) ++ "]"
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
  TermProduct els -> "(" ++ (L.intercalate ", " $ fmap showTerm els) ++ ")"
  TermTypeAbstraction (TypeAbstraction (Name param) body) -> "Λ" ++ param ++ "." ++ showTerm body
  TermTypeApplication (TypedTerm term typ) -> showTerm term ++ "⟨" ++ showType typ ++ "⟩"
  TermVariable (Name name) -> name
  TermWrap (WrappedTerm tname term1) -> "wrap[" ++ unName tname ++ "](" ++ showTerm term1 ++ ")"
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
  TypeProduct types -> L.intercalate "×" (fmap showType types)
  TypeVariable (Name name) -> name
  TypeWrap (WrappedType tname typ1) -> "wrap[" ++ unName tname ++ "](" ++ showType typ1 ++ ")"
  t -> show t

showTypeScheme :: TypeScheme -> String
showTypeScheme (TypeScheme vars body) = fa ++ showType body
  where
    fa = if L.null vars then "" else "∀[" ++ (L.intercalate "," (fmap (\(Name name) -> name) vars)) ++ "]."

showTypeConstraint :: TypeConstraint -> String
showTypeConstraint (TypeConstraint ltyp rtyp _) = showType ltyp ++ "≡" ++ showType rtyp

showTypeSubst :: TypeSubst -> String
showTypeSubst (TypeSubst subst) = "{" ++ (L.intercalate "," (fmap (\(Name name, typ) -> name ++ "↦" ++ showType typ) (M.toList subst))) ++ "}"

--------------------------------------------------------------------------------
-- Unification

-- TODO: eliminate lambda types before unification
-- TODO: reduce application types before unification

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
unifyTypeConstraints :: [TypeConstraint] -> Flow s TypeSubst
unifyTypeConstraints constraints = case constraints of
    [] -> Flows.pure emptyTypeSubst
    ((TypeConstraint left right comment):rest) -> do
        result <- case sleft of
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

--        if L.length constraints == 3
--          then debug "All tests, Inference tests, Simple terms, List terms, List with bound variables, #1" $ ""
--            ++ "\n\tconstraints: " ++ (L.intercalate ", " $ fmap showTypeConstraint constraints)
--            ++ "\n\tresult: " ++ showTypeSubst result
--          else Flows.pure ()

        return result
      where
        sleft = stripType left
        sright = stripType right
        -- TODO: this occurrence check is expensive; consider delaying it until the time of substitution
        tryBinding v t = if variableOccursInType v t
          then Flows.fail $ "Variable " ++ unName v ++ " apears free in " ++ showType t
            ++ (Y.maybe "" (\c -> " (" ++ c ++ ")") comment)
          else bind v t
        bind v t = composeTypeSubst subst <$> unifyTypeConstraints (substituteInConstraints subst rest)
          where
            subst = singletonTypeSubst v t

unifyTypeLists :: [Type] -> [Type] -> Maybe String -> Flow s TypeSubst
unifyTypeLists l r comment = unifyTypeConstraints $ L.zipWith toConstraint l r
  where
    toConstraint l r = TypeConstraint l r comment

unifyTypes :: Type -> Type -> Maybe String -> Flow s TypeSubst
unifyTypes l r comment = unifyTypeConstraints [TypeConstraint l r comment]

joinTypes :: Type -> Type -> Maybe String -> Flow s [TypeConstraint]
joinTypes left right comment = case sleft of
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
    -- TypeAnnotated, TypeApplication, TypeLambda, TypeVariable should not appear here
    _ -> cannotUnify
  where
    sleft = stripType left
    sright = stripType right
    joinOne l r = TypeConstraint l r Nothing
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

data TypeSubst = TypeSubst { unTypeSubst :: M.Map Name Type } deriving (Eq, Ord)
data TermSubst = TermSubst { unTermSubst :: M.Map Name Term } deriving (Eq, Ord)

instance Show TypeSubst where
  show (TypeSubst subst) = "{" ++ L.intercalate ", " (fmap (\((Name k), v) -> k ++ ": " ++ showType v) $ M.toList subst) ++ "}"

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
substInContext subst (AltInferenceContext types) = AltInferenceContext $ fmap (substInTypeScheme subst) types

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
    altInferenceContextTypes :: M.Map Name TypeScheme}
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
    ++ "constraints= [" ++ (L.intercalate ", " $ fmap showTypeConstraint constraints) ++ "], "
    ++ "subst= " ++ showTypeSubst subst ++ "}"

dummyTerm :: Term
dummyTerm = TermLiteral $ LiteralString "dummy"

freeVariablesInContext :: AltInferenceContext -> S.Set Name
freeVariablesInContext cx = L.foldl S.union S.empty $ fmap freeVariablesInTypeSchemeSimple $ M.elems $ altInferenceContextTypes cx

-- | Warning: this is an expensive operation (linear with respect to the size of the graph and schema graph)
generalize :: AltInferenceContext -> Type -> TypeScheme
generalize cx typ = TypeScheme vars typ
  where
    vars = L.nub $ L.filter isUnbound $ S.toList $ freeVariablesInType typ
    isUnbound v = not $ S.member v $ freeVariablesInContext cx

inferMany :: AltInferenceContext -> [Term] -> Flow s ([Term], [Type], TypeSubst)
inferMany cx terms = case terms of
  [] -> return ([], [], emptyTypeSubst)
  (e:tl) -> do
    (AltInferenceResult e1 t1 _ s1) <- inferTypeOfTerm cx e
    (e2, t2, s2) <- inferMany (substInContext s1 cx) tl
    return (
      (substTypesInTerm s2 e1):e2,
      (substInType s2 $ typeSchemeType t1):t2,
      composeTypeSubst s2 s1)
-- TODO: restore the simpler and more efficient parallel implementation (no substInContext) if it does not impact correctness
--inferMany cx terms = Flows.map withResults $ Flows.sequence $ fmap (inferTypeOfTerm cx) terms
--  where
--    withResults results = (
--      fmap altInferenceResultTerm results,
--      fmap (typeSchemeType . altInferenceResultTypeScheme) results,
--      composeTypeSubstList $ fmap altInferenceResultSubst results)

inferTypeOf :: AltInferenceContext -> Term -> Flow s TypeScheme
inferTypeOf cx term = bindInferredTerm cx letTerm unifyAndSubst
  where
    letTerm = TermLet $ Let [LetBinding (Name "x") term Nothing] $ Terms.string "ignored"
    unifyAndSubst result = do
        (Let bindings _) <- Expect.letTerm $ altInferenceResultTerm result
        case bindings of
          [LetBinding _ _ (Just ts)] -> Flows.map (doSubst ts) (unifyTypeConstraints $ altInferenceResultConstraints result)
          _ -> Flows.fail $ "Expected a single binding with a type scheme, but got: " ++ show bindings
      where
        subst1 = altInferenceResultSubst result
        -- TODO: get rid of subst2 along with passed constraints
        doSubst ts subst2 = normalizeVariablesInTypeScheme $ substInTypeScheme subst ts
          where
            subst = composeTypeSubst subst1 subst2

inferTypeOfAnnotatedTerm :: AltInferenceContext -> AnnotatedTerm -> Flow s AltInferenceResult
inferTypeOfAnnotatedTerm cx (AnnotatedTerm term _) = inferTypeOfTerm cx term

inferTypeOfApplication :: AltInferenceContext -> Application -> Flow s AltInferenceResult
inferTypeOfApplication cx (Application e0 e1) = bindInferredTerm cx e0 withLhs
  where
    withLhs (AltInferenceResult a t0 _ s0) = bindInferredTerm (substInContext s0 cx) e1 withRhs
      where
        withRhs (AltInferenceResult b t1 _ s1) = bindVar withVar
          where
            withVar v = Flows.map withSubst
                $ unifyTypes (substInType s1 $ typeSchemeType t0) (Types.function (typeSchemeType t1) $ TypeVariable v) $ Just "application lhs"
              where
                withSubst s2 = AltInferenceResult rExpr (Types.mono rType) [] rSubst
                  where
                    rExpr = Terms.apply (substTypesInTerm (composeTypeSubst s2 s1) a) (substTypesInTerm s2 b)
                    rType = substInType s2 $ TypeVariable v
                    rSubst = composeTypeSubstList [s2, s1, s0]

inferTypeOfCaseStatement :: AltInferenceContext -> CaseStatement -> Flow s AltInferenceResult
inferTypeOfCaseStatement cx (CaseStatement tname dflt cases) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    fnames = fmap fieldName cases
    withSchemaType (TypeScheme _ styp) = Flows.bind (expectUnionType tname styp) withFields
      where
        withFields sfields = bind2 (traverse (inferTypeOfTerm cx) dflt) (inferMany cx $ fmap fieldTerm cases) withResults
          where
            withResults mr (iterms, itypes, isubst) = bindVar withCod
              where
                withCod codv = mapConstraints withConstraints (dfltConstraints ++ caseConstraints)
                  where
                    cod = TypeVariable codv
                    dfltConstraints = maybeToList $ fmap (\r -> TypeConstraint cod (typeSchemeType $ altInferenceResultTypeScheme r) $ Just "match default") mr
                    caseConstraints = Y.catMaybes $ L.zipWith (\fname itype -> fmap (\r -> toConstraint r itype) $ M.lookup fname caseMap) fnames itypes
                      where
                        caseMap = M.fromList $ fmap (\(FieldType fname ftype) -> (fname, ftype)) sfields
                        toConstraint ftyp r = TypeConstraint r (Types.function ftyp cod) $ Just "case type"
                    withConstraints subst = yield
                        (TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement tname (fmap altInferenceResultTerm mr) $ L.zipWith Field fnames iterms)
                        (TypeFunction $ FunctionType styp cod)
                        (composeTypeSubstList $ (maybeToList $ fmap altInferenceResultSubst mr) ++ [isubst, subst])

inferTypeOfCollection :: AltInferenceContext -> (Type -> Type) -> ([Term] -> Term) -> String -> [Term] -> Flow s AltInferenceResult
inferTypeOfCollection cx typCons trmCons desc els = bindVar withVar
  where
    withVar var = Flows.bind (inferMany cx els) fromResults
      where
        fromResults (terms, types, subst1) = bindConstraints constraints withConstraints
          where
            constraints = fmap (\t -> TypeConstraint (TypeVariable var) t $ Just desc) types
            withConstraints subst2 = do

--                debug "All tests, Inference tests, Simple terms, List terms, List with bound variables, #1" $ ""
--                    ++ "\n\tels: " ++ (L.intercalate ", " $ fmap showTerm els)
--                    ++ "\n\trExpr: " ++ showTerm rExpr
--                    ++ "\n\trType: " ++ showType rType
--                    ++ "\n\tsubst1: " ++ show subst1
--                    ++ "\n\tsubst2: " ++ show subst2
--                    ++ "\n\tsubst: " ++ show subst

                return $ yield (trmCons terms) (typCons $ TypeVariable var) $ composeTypeSubst subst2 subst1

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
        aT = TypeVariable a
        bT = TypeVariable b
        withResult (AltInferenceResult iterm (TypeScheme _ ityp) _ isubst) = mapConstraints withSubst [
            TypeConstraint expectedType ityp $ Just "fold function"]
          where
            expectedType = Types.functionN [bT, aT, bT]
            withSubst subst = yield iterm (Types.functionN [bT, Types.list aT, bT]) $ composeTypeSubst isubst subst

inferTypeOfFunction :: AltInferenceContext -> Function -> Flow s AltInferenceResult
inferTypeOfFunction cx f = case f of
  FunctionElimination elm -> inferTypeOfElimination cx elm
  FunctionLambda l -> inferTypeOfLambda cx l
  FunctionPrimitive name -> inferTypeOfPrimitive cx name

inferTypeOfInjection :: AltInferenceContext -> Injection -> Flow s AltInferenceResult
inferTypeOfInjection cx (Injection tname (Field fname term)) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term) withResults
  where
    withResults (TypeScheme _ styp) (AltInferenceResult iterm (TypeScheme _ ityp) _ isubst) =
        Flows.bind (expectUnionType tname styp) withFields
      where
        withFields sfields = Flows.bind (findFieldType fname sfields) withField
          where
            withField ftyp = mapConstraints withSubst [
                TypeConstraint ftyp ityp $ Just "schema type of injected field"]
              where
                  withSubst subst = yield (Terms.inject tname $ Field fname iterm) styp $ composeTypeSubst isubst subst

inferTypeOfLambda :: AltInferenceContext -> Lambda -> Flow s AltInferenceResult
inferTypeOfLambda cx tmp@(Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = Flows.map withResult (inferTypeOfTerm cx2 body)
      where
        dom = TypeVariable vdom
        cx2 = extendContext [(var, Types.mono $ TypeVariable vdom)] cx
        withResult (AltInferenceResult iterm (TypeScheme _ itype) _ isubst) =
          yield (TermFunction $ FunctionLambda $ Lambda var (Just dom) iterm) (Types.function dom itype) isubst

-- TODO: this rule contains some expensive operations. Various optimizations may be possible, such as saving substitutions until the end (outside of the rule itself).
inferTypeOfLet :: AltInferenceContext -> Let -> Flow s AltInferenceResult
inferTypeOfLet cx (Let b0 env0) = bindVars (L.length b0) withVars
  where
    bnames = fmap letBindingName b0
    eb0 = fmap letBindingTerm b0
    withVars bvars = Flows.bind (inferMany cx2 eb0) withInferredBindings
      where
        cx2 = extendContext (L.zip bnames $ fmap Types.mono btypes) cx
        btypes = fmap TypeVariable bvars
        withInferredBindings (eb1, tb, s1) = Flows.bind
            (unifyTypeLists (fmap (substInType s1) btypes) tb $ Just "let temporary type bindings") withSubst
          where
            withSubst sb = Flows.bind (inferTypeOfTerm cx3 env0) withEnv
              where
--                cx3 = extendContext (L.zip bnames $ fmap (generalize cx . substInType sb) tb) cx2 -- TODO: restore me; this is probably more correct. Add test cases for nested let, e.g. let x = 42 in let y = 37 in x+y
                cx3 = AltInferenceContext $ M.fromList $ L.zip bnames $ fmap (generalize cx . substInType sb) tb
                withEnv tmp@(AltInferenceResult env1 tenv _ senv) = do

--                    if (TermFunction $ FunctionLambda tmp) == (Terms.lambda "v" $ Terms.wrap (Name "StringTypeAlias") $ Terms.var "v")
--                      then Flows.fail $ "bnames: " ++ L.intercalate ", " (fmap unName bnames)
--                        ++ "\nbvars: " ++ L.intercalate ", " (fmap unName bvars)
--                        ++ "\nenv result: " ++ show tmp
--                        ++ "\nbinding result terms:" ++ (L.concat $ L.zipWith (\n r -> "\n\t" ++ unName n ++ ": " ++ showTerm r) bnames eb1)
--                        ++ "\nbinding result types:" ++ (L.concat $ L.zipWith (\n r -> "\n\t" ++ unName n ++ ": " ++ showType r) bnames tb)
--                        ++ "\nbinding result subst: " ++ show s1
--                        ++ "\noverall result: " ++ show (AltInferenceResult let1 tenv [] s2)
--                      else Flows.pure ()

                    return $ AltInferenceResult let1 tenv [] s2
                  where
                    s2 = composeTypeSubstList [senv, sb, s1]
                    eb2 = fmap (substTypesInTerm $ composeTypeSubst senv sb) eb1
                    b3t = L.zip bnames $ fmap (generalize cx . substInType sb) tb
--                    b3t = L.zip bnames $ fmap (generalize cx . substInType s2) tb
                    s3 = TermSubst $ M.fromList $ fmap (\(x, ts) -> (x, (typeApplication (TermVariable x) $ fmap TypeVariable $ typeSchemeVariables ts))) b3t
                    b3 = L.zipWith (\(x, ts) e -> LetBinding x
                      (substTypesInTerm (composeTypeSubst sb senv) $ typeAbstraction (typeSchemeVariables ts) $ substituteInTerm s3 e)
                      (Just $ substInTypeScheme senv ts)) b3t eb2
                    let1 = TermLet $ Let b3 env1

inferTypeOfList :: AltInferenceContext -> [Term] -> Flow s AltInferenceResult
inferTypeOfList cx = inferTypeOfCollection cx Types.list Terms.list "list element"

inferTypeOfLiteral :: AltInferenceContext -> Literal -> Flow s AltInferenceResult
inferTypeOfLiteral _ lit = Flows.pure $ AltInferenceResult (TermLiteral lit) (Types.mono $ TypeLiteral $ literalType lit) [] emptyTypeSubst

inferTypeOfMap :: AltInferenceContext -> M.Map Term Term -> Flow s AltInferenceResult
inferTypeOfMap cx m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        -- TODO: get rid of this special case; it should follow from the generate case
        then Flows.pure $ yield (TermMap M.empty) (Types.map (TypeVariable kvar) (TypeVariable vvar)) emptyTypeSubst
        else Flows.bind (inferMany cx $ M.keys m) withKeys
      where
        withKeys (kterms, ktypes, ksubst) = Flows.bind (inferMany cx $ M.elems m) withValues
          where
            withValues (vterms, vtypes, vsubst) = mapConstraints withSubst $ kcons ++ vcons
              where
                kcons = fmap (\t -> TypeConstraint (TypeVariable kvar) t $ Just "map key") ktypes
                vcons = fmap (\t -> TypeConstraint (TypeVariable vvar) t $ Just "map value") vtypes
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
    withVars domv codv = bindInferredTerm2 cx n j withResults
      where
        dom = TypeVariable domv
        cod = TypeVariable codv
        withResults nresult jresult = mapConstraints withSubst [
            TypeConstraint cod ntyp $ Just "optional elimination; nothing case",
            TypeConstraint (Types.function dom cod) jtyp $ Just "optional elimination; just case"]
          where
            ntyp = typeSchemeType $ altInferenceResultTypeScheme nresult
            jtyp = typeSchemeType $ altInferenceResultTypeScheme jresult
            withSubst subst = yield
                (Terms.matchOpt (altInferenceResultTerm nresult) (altInferenceResultTerm jresult))
                (Types.function (Types.optional dom) cod)
                (composeTypeSubstList [altInferenceResultSubst nresult, altInferenceResultSubst jresult, subst])

inferTypeOfPrimitive :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfPrimitive cx name = case M.lookup name (altInferenceContextTypes cx) of
    Nothing -> Flows.fail $ "No such primitive: " ++ unName name
    -- TODO: check against algo W implementation
    Just scheme -> Flows.map withScheme $ instantiateTypeScheme scheme
  where
    withScheme ts = yield (Terms.primitive name) (typeSchemeType ts) emptyTypeSubst

inferTypeOfProduct :: AltInferenceContext -> [Term] -> Flow s AltInferenceResult
inferTypeOfProduct cx els = Flows.map withResults (inferMany cx els)
  where
    withResults (iterms, itypes, isubst) = yield (Terms.product iterms) (Types.product itypes) isubst

inferTypeOfProjection :: AltInferenceContext -> Projection -> Flow s AltInferenceResult
inferTypeOfProjection cx (Projection tname fname) = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme _ styp) = Flows.bind (expectRecordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ findFieldType fname sfields
          where
            withField ftyp = yield (Terms.project tname fname) (Types.function styp ftyp) emptyTypeSubst

inferTypeOfRecord :: AltInferenceContext -> Record -> Flow s AltInferenceResult
inferTypeOfRecord cx (Record tname fields) =
    bind2 (requireSchemaType cx tname) (inferMany cx $ fmap fieldTerm fields) withResults
  where
    fnames = fmap fieldName fields
    withResults (TypeScheme _ styp) (iterms, itypes, isubst) = mapConstraints withSubst [
        TypeConstraint styp ityp $ Just "schema type of record"]
      where
        ityp = TypeRecord $ RowType tname $ L.zipWith FieldType fnames itypes
        withSubst subst = yield
            (TermRecord $ Record tname $ L.zipWith Field fnames iterms)
            ityp
            (composeTypeSubst isubst subst)

inferTypeOfSet :: AltInferenceContext -> S.Set Term -> Flow s AltInferenceResult
inferTypeOfSet cx = inferTypeOfCollection cx Types.set (Terms.set . S.fromList) "set element" . S.toList

inferTypeOfSum :: AltInferenceContext -> Sum -> Flow s AltInferenceResult
inferTypeOfSum cx (Sum i s term) = bindInferredTerm cx term withResult
  where
    withResult (AltInferenceResult iterm (TypeScheme _ typ) icons isubst) = Flows.map withVars (Flows.sequence $ fmap (varOrTerm typ) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then Flows.pure $ Left t
          else Right <$> freshName
        withVars vars = yield (TermSum $ Sum i s iterm) (TypeSum $ fmap toType vars) isubst
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
    withVars vars = yield
        (Terms.untuple arity idx)
        (Types.function (Types.product types) cod)
        emptyTypeSubst
      where
        types = TypeVariable <$> vars
        cod = types !! idx

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: AltInferenceContext -> TypedTerm -> Flow s AltInferenceResult
inferTypeOfTypedTerm cx (TypedTerm term _) = inferTypeOfTerm cx term

inferTypeOfUnwrap :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfUnwrap cx tname = Flows.bind (requireSchemaType cx tname) withSchemaType
  where
    withSchemaType (TypeScheme _ styp) = Flows.map withWrappedType (expectWrappedType tname styp)
      where
        withWrappedType wtyp = yield
          (Terms.unwrap tname)
          (Types.function styp wtyp)
          emptyTypeSubst

inferTypeOfVariable :: AltInferenceContext -> Name -> Flow s AltInferenceResult
inferTypeOfVariable cx name = case M.lookup name (altInferenceContextTypes cx) of
    Nothing -> Flows.fail $ "Variable not bound to type: " ++ unName name
    Just scheme -> Flows.map withTypeScheme $ instantiateTypeScheme scheme
  where
    withTypeScheme (TypeScheme vars typ) = AltInferenceResult
      (typeApplication (TermVariable name) $ fmap TypeVariable vars)
      (Types.mono typ)
      [] emptyTypeSubst

inferTypeOfWrappedTerm :: AltInferenceContext -> WrappedTerm -> Flow s AltInferenceResult
inferTypeOfWrappedTerm cx (WrappedTerm tname term) = bind2 (requireSchemaType cx tname) (inferTypeOfTerm cx term) withResult
  where
    withResult (TypeScheme _ styp) (AltInferenceResult iterm (TypeScheme _ ityp) icons isubst) = mapConstraints withSubst [
        TypeConstraint styp (TypeWrap $ WrappedType tname ityp) $ Just "schema type of wrapper"]
      where
        withSubst subst = yield (Terms.wrap tname iterm) styp (composeTypeSubst isubst subst)

--------------------------------------------------------------------------------

bindConstraints :: [TypeConstraint] -> (TypeSubst -> Flow s a) -> Flow s a
bindConstraints constraints = Flows.bind (unifyTypeConstraints constraints)

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

instantiateTypeSchemeLegacy :: TypeScheme -> Flow s TypeScheme
instantiateTypeSchemeLegacy scheme = Flows.map doSubst (freshNames $ L.length oldVars)
  where
    doSubst newVars = TypeScheme newVars $ substInType subst $ typeSchemeType scheme
      where
        subst = TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)
    oldVars = L.intersect (L.nub $ typeSchemeVariables scheme) (freeVariablesInSimpleType $ typeSchemeType scheme)

instantiateTypeSchemeAndNormalize :: TypeScheme -> Flow s TypeScheme
instantiateTypeSchemeAndNormalize scheme = Flows.map normalizeVariablesInTypeScheme (instantiateTypeSchemeLegacy scheme)

-- Note: does not account for "lambda types"; here we use type schemes instead.
freeVariablesInSimpleType :: Type -> [Name]
freeVariablesInSimpleType = L.nub . foldOverType TraversalOrderPre fold []
  where
    fold rest typ = case typ of
      TypeVariable name -> name : rest
      _ -> rest

mapConstraints :: (TypeSubst -> a) -> [TypeConstraint] -> Flow s a
mapConstraints f constraints = Flows.map f $ unifyTypeConstraints constraints

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

yield :: Term -> Type -> TypeSubst -> AltInferenceResult
yield term typ subst = AltInferenceResult (substTypesInTerm subst term) (Types.mono $ substInType subst typ) [] subst
