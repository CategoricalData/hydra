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
-- Graphs

showType :: Type -> String
showType (TypeFunction (FunctionType dom cod)) = "(" ++ showType dom ++ "→" ++ showType cod ++ ")"
showType (TypeList etyp) = "[" ++ showType etyp ++ "]"
showType (TypeLiteral lit) = show lit
showType (TypeMap (MapType keyTyp valTyp)) = "map<" ++ showType keyTyp ++ "," ++ showType valTyp ++ ">"
showType (TypeProduct types) = "(" ++ (L.intercalate "," (fmap showType types)) ++ ")"
showType (TypeVariable (Name name)) = name

showTypeScheme :: TypeScheme -> String
showTypeScheme (TypeScheme vars body) = "∀[" ++ (L.intercalate "," (fmap (\(Name name) -> name) vars)) ++ "]." ++ showType body

showConstraint :: TypeConstraint -> String
showConstraint (TypeConstraint ltyp rtyp _) = showType ltyp ++ "≡" ++ showType rtyp

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

unifyTypeLists :: [Type] -> [Type] -> Maybe String -> Either UnificationError TypeSubst
unifyTypeLists l r comment = joinTypeLists l r comment >>= unifyTypeConstraints

joinTypeLists :: [Type] -> [Type] -> Maybe String -> Either UnificationError [TypeConstraint]
joinTypeLists l r comment = L.concat <$> CM.zipWithM (\t1 t2 -> joinTypes t1 t2 comment) l r

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
          rewriteBinding (LetBinding v e mt) = LetBinding v e $ fmap (substituteInTypeScheme subst) mt
      TermTypeAbstraction (TypeAbstraction param body) -> TermTypeAbstraction $ TypeAbstraction param $ substituteTypesInTerm subst2 body
        where
          subst2 = TypeSubst $ M.delete param $ unTypeSubst subst
      TermTypeApplication (TypedTerm trm typ) -> recurse $ TermTypeApplication $ TypedTerm trm $ substituteInType subst typ
      _ -> recurse term

--------------------------------------------------------------------------------
-- Inference

data AltInferenceContext
  = AltInferenceContext {
    altInferenceContextVariableCount :: Int,
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
  show (AltInferenceResult term scheme constraints subst) = "{term=" ++ show term ++ ", type= " ++ showTypeScheme scheme ++ ", constraints= " ++ show constraints ++ ", subst= " ++ show subst ++ "}"

dummyTerm :: Term
dummyTerm = TermLiteral $ LiteralString "dummy"

-- | Warning: this is an expensive operation (linear with respect to the size of the graph and schema graph)
generalize :: AltInferenceContext -> Type -> TypeScheme
generalize cx typ = TypeScheme vars typ
  where
    vars = L.nub $ L.filter isUnbound $ S.toList $ freeVariablesInType typ
    isUnbound v = not $ S.member v freeVars
    freeVars = L.foldl S.union S.empty $ fmap freeVariablesInTypeScheme $ M.elems $ altInferenceContextTypes cx

inferTypeOf :: Term -> Flow AltInferenceContext TypeScheme
inferTypeOf term = bindInferredTerm term unifyAndSubst
  where
    unifyAndSubst :: AltInferenceResult -> Flow AltInferenceContext TypeScheme
    unifyAndSubst result = Flows.bind (TM.fromEither $ unifyTypeConstraints $ altInferenceResultConstraints result) doSubst
      where
        doSubst :: TypeSubst -> Flow AltInferenceContext TypeScheme
        doSubst subst = instantiateTypeSchemeAndNormalize $ substituteInTypeScheme subst $ altInferenceResultTypeScheme result

inferTypeOfAnnotatedTerm :: AnnotatedTerm -> Flow AltInferenceContext AltInferenceResult
inferTypeOfAnnotatedTerm (AnnotatedTerm term _) = inferTypeOfTerm term

inferTypeOfApplication :: Application -> Flow AltInferenceContext AltInferenceResult
inferTypeOfApplication (Application lterm rterm) = bindVar2 withVars
  where
    withVars dom cod = bind2 (inferTypeOfTerm lterm) (inferTypeOfTerm rterm) withResults
      where
        withResults lresult rresult = mapConstraints withConstraints [
            TypeConstraint (Types.function (TypeVariable dom) (TypeVariable cod)) ltyp $ Just "application; lhs",
            TypeConstraint (TypeVariable dom) rtyp $ Just "application; rhs"]
          where
            withConstraints subst = AltInferenceResult
                dummyTerm
                (TypeScheme tvars $ TypeVariable cod)
                ([TypeConstraint (Types.function (TypeVariable dom) (TypeVariable cod)) ltyp $ Just "application; lhs",
                  TypeConstraint (TypeVariable dom) rtyp $ Just "application; rhs"]
                 ++ altInferenceResultConstraints lresult ++ altInferenceResultConstraints rresult)
                (composeTypeSubstList [subst, altInferenceResultSubst lresult, altInferenceResultSubst rresult])
            ltyp = typeSchemeType $ altInferenceResultTypeScheme lresult
            rtyp = typeSchemeType $ altInferenceResultTypeScheme rresult
            tvars = typeSchemeVariables (altInferenceResultTypeScheme lresult) ++ typeSchemeVariables (altInferenceResultTypeScheme rresult)

inferTypeOfCaseStatement :: CaseStatement -> Flow AltInferenceContext AltInferenceResult
inferTypeOfCaseStatement (CaseStatement tname dflt cases) = Flows.bind (requireSchemaType tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (expectUnionType tname styp) withFields
      where
        withFields sfields = bind2 (traverse inferTypeOfTerm dflt) (Flows.sequence $ fmap inferTypeOfTerm $ fmap fieldTerm cases) withResults
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

inferTypeOfCollection :: (Type -> Type) -> String -> [Term] -> Flow AltInferenceContext AltInferenceResult
inferTypeOfCollection cons desc els = bindVar withVar
  where
    withVar var = forInferredTerms els fromResults
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

inferTypeOfElimination :: Elimination -> Flow AltInferenceContext AltInferenceResult
inferTypeOfElimination elm = case elm of
  EliminationList fun -> inferTypeOfFold fun
  EliminationOptional oc -> inferTypeOfOptionalCases oc
  EliminationProduct tp -> inferTypeOfTupleProjection tp
  EliminationRecord p -> inferTypeOfProjection p
  EliminationUnion c -> inferTypeOfCaseStatement c
  EliminationWrap tname -> inferTypeOfUnwrap tname

inferTypeOfFold :: Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfFold fun = bindVar2 withVars
  where
    withVars a b = Flows.bind (inferTypeOfTerm fun) withResult
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

inferTypeOfFunction :: Function -> Flow AltInferenceContext AltInferenceResult
inferTypeOfFunction f = case f of
  FunctionElimination elm -> inferTypeOfElimination elm
  FunctionLambda l -> inferTypeOfLambda l
  FunctionPrimitive name -> inferTypeOfPrimitive name

inferTypeOfInjection :: Injection -> Flow AltInferenceContext AltInferenceResult
inferTypeOfInjection (Injection tname (Field fname term)) = bind2 (requireSchemaType tname) (inferTypeOfTerm term) withResults
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

inferTypeOfLambda :: Lambda -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLambda (Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = withTypeBindings [(var, TypeScheme [] $ TypeVariable vdom)]
        $ Flows.map withResult (inferTypeOfTerm body)
      where
        withResult (AltInferenceResult _ (TypeScheme vars t) constraints subst)
          = AltInferenceResult dummyTerm (TypeScheme (vdom:vars) $ Types.function (TypeVariable vdom) t) constraints subst






inferTypeOfLet :: Let -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLet = inferTypeOfLetOld

inferTypeOfLetNew :: Let -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLetNew (Let b0 env0) = do
    g <- getState
    bindVars (L.length b0) (withVars g)
  where
    bnames = fmap letBindingName b0
    eb0 = fmap letBindingTerm b0
    withVars g bvars = withTypeBindings (L.zip bnames $ fmap (TypeScheme []) btypes) withTmpBindings1
      where
        btypes = fmap TypeVariable bvars
        withTmpBindings1 = Flows.bind (Flows.sequence $ fmap inferTypeOfTerm eb0) withInferredBindings
          where
            withInferredBindings results = Flows.bind
                (TM.fromEither $ unifyTypeLists (fmap (substituteInType s1) btypes) tb $ Just "let temporary type bindings") withSubst
              where
                eb1 = fmap altInferenceResultTerm results
                tb = fmap (typeSchemeType . altInferenceResultTypeScheme) results
                s1 = composeTypeSubstList $ fmap altInferenceResultSubst results
                withSubst sb = withTypeBindings (L.zip bnames $ fmap (generalize g . substituteInType sb) tb) withTmpBindings2
                  where
                    withTmpBindings2 = Flows.map withEnv $ inferTypeOfTerm env0
                      where
                        withEnv (AltInferenceResult env1 tenv _ senv) = AltInferenceResult let1 tenv [] s2
                          where
                            s2 = composeTypeSubstList [senv, sb, s1]
                            eb2 = fmap (substituteTypesInTerm $ composeTypeSubst senv sb) eb1
                            b3t = L.zip bnames $ fmap (generalize g . substituteInType s2) tb
                            s3 = TermSubst $ M.fromList $ fmap (\(x, ts) -> (x, (typeApplication (TermVariable x) $ fmap TypeVariable $ typeSchemeVariables ts))) b3t
                            b3 = L.zipWith (\(x, ts) e -> LetBinding x
                              (substituteTypesInTerm (composeTypeSubst sb senv) $ typeAbstraction (typeSchemeVariables ts) $ substituteInTerm s3 e)
                              (Just $ substituteInTypeScheme senv ts)) b3t eb2
                            let1 = TermLet $ Let b3 env1

inferTypeOfLetOld :: Let -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLetOld (Let bindings env) = bindVars (L.length bindings) withVars
  where
    bindingNames = letBindingName <$> bindings
    bindingTerms = letBindingTerm <$> bindings
    withVars bvars = withTypeBindings (L.zip bindingNames $ fmap (TypeScheme []) btypes) withExtendedEnv1
      where
        btypes = fmap TypeVariable bvars
        withExtendedEnv1 = Flows.bind (Flows.sequence $ fmap inferTypeOfTerm bindingTerms) withInferredBindings
          where
            withInferredBindings rbindings = withTypeBindings (L.zip bindingNames $ fmap altInferenceResultTypeScheme rbindings) inferEnv
              where
                inferEnv = Flows.map withInferredEnv (inferTypeOfTerm env)
                  where
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
inferTypeOfLetOlder :: Let -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLetOlder (Let bindings env) = if L.null bindings
    then Flows.map forEmptyBindings (inferTypeOfTerm env)
    else if L.length bindings > 2
    then inferTypeOfTerm $ TermLet (Let [L.head bindings] $ TermLet $ Let (L.tail bindings) env)
    else forSingleBinding $ L.head bindings
  where
    forEmptyBindings r = r
    forSingleBinding (LetBinding key value _) = bindVar withVar
      where
        -- Create a temporary type variable for the binding
        withVar var = withTypeBindings [(key, Types.mono $ TypeVariable var)] $
            bindInferredTerm value withValueType
          where
            -- Unify and substitute over the value constraints
            -- TODO: save the substitution and pass it along, instead of the original set of constraints
            withValueType (AltInferenceResult _ rawValueScheme valueConstraints _) = Flows.bind (TM.fromEither $ unifyTypeConstraints kvConstraints) afterUnification
              where
                rawValueVars = typeSchemeVariables rawValueScheme
                kvConstraints = keyConstraint:valueConstraints
                keyConstraint = TypeConstraint (TypeVariable var) (typeSchemeType rawValueScheme) $ Just "let binding"
                -- Now update the type binding to use the inferred type
                afterUnification subst = withTypeBindings [(key, valueScheme)]
                    $ Flows.map withEnvType (inferTypeOfTerm env)
                  where
                    valueScheme = substituteInTypeScheme subst rawValueScheme
                    withEnvType (AltInferenceResult _ envScheme envConstraints _) = AltInferenceResult dummyTerm envScheme constraints emptyTypeSubst
                      where
                        constraints = kvConstraints ++ envConstraints
                        envVars = typeSchemeVariables envScheme








inferTypeOfList :: [Term] -> Flow AltInferenceContext AltInferenceResult
inferTypeOfList = inferTypeOfCollection Types.list "list element"

inferTypeOfLiteral :: Literal -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLiteral lit = Flows.pure $ AltInferenceResult dummyTerm (Types.mono $ TypeLiteral $ literalType lit) [] emptyTypeSubst

inferTypeOfMap :: M.Map Term Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfMap m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        -- TODO: get rid of this special case; it should follow from the generate case
        then Flows.pure $ AltInferenceResult dummyTerm (TypeScheme [kvar, vvar] $ Types.map (TypeVariable kvar) (TypeVariable vvar)) [] emptyTypeSubst
        else Flows.map withResults (Flows.sequence $ fmap fromPair $ M.toList m)
      where
        fromPair :: (Term, Term) -> Flow AltInferenceContext (([Name], [TypeConstraint]), TypeSubst)
        fromPair (k, v) = bindInferredTerm2 k v withPairResults
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

inferTypeOfOptional :: Maybe Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfOptional m = inferTypeOfCollection Types.optional "optional element" $ Y.maybe [] (\e -> [e]) m

inferTypeOfOptionalCases :: OptionalCases -> Flow AltInferenceContext AltInferenceResult
inferTypeOfOptionalCases (OptionalCases n j) = bindVar2 withVars
  where
    withVars domv codv = bindInferredTerm2 n j withResults
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

inferTypeOfPrimitive :: Name -> Flow AltInferenceContext AltInferenceResult
inferTypeOfPrimitive name = Flow $ \ctx t -> case M.lookup name (altInferenceContextTypes ctx) of
  Just scheme -> unFlow (Flows.map withoutConstraints $ instantiateTypeScheme scheme) ctx t
  Nothing -> unFlow (Flows.fail $ "No such primitive: " ++ unName name) ctx t

inferTypeOfProduct :: [Term] -> Flow AltInferenceContext AltInferenceResult
inferTypeOfProduct els = if L.null els
    -- TODO: get rid of this special case; it should follow from the generate case
    then Flows.pure $ AltInferenceResult dummyTerm (Types.mono $ Types.product []) [] emptyTypeSubst
    else forInferredTerms els fromResults
  where
    fromResults results = AltInferenceResult dummyTerm (TypeScheme tvars $ TypeProduct tbodies) constraints subst
      where
        tvars = L.concat $ typeSchemeVariables . altInferenceResultTypeScheme <$> results
        tbodies = fmap (typeSchemeType . altInferenceResultTypeScheme) results
        constraints = L.concat $ fmap altInferenceResultConstraints results
        subst = composeTypeSubstList $ fmap altInferenceResultSubst results

inferTypeOfProjection :: Projection -> Flow AltInferenceContext AltInferenceResult
inferTypeOfProjection (Projection tname fname) = Flows.bind (requireSchemaType tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.bind (expectRecordType tname styp) withRecordType
      where
        withRecordType sfields = Flows.map withField $ findFieldType fname sfields
          where
            withField ftyp = AltInferenceResult dummyTerm (TypeScheme svars $ Types.function styp ftyp) [] emptyTypeSubst

inferTypeOfRecord :: Record -> Flow AltInferenceContext AltInferenceResult
inferTypeOfRecord (Record tname fields) = bind2
    (requireSchemaType tname)
    (Flows.sequence $ fmap inferTypeOfTerm $ fmap fieldTerm fields)
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

inferTypeOfSet :: S.Set Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfSet = inferTypeOfCollection Types.set "set element" . S.toList

inferTypeOfSum :: Sum -> Flow AltInferenceContext AltInferenceResult
inferTypeOfSum (Sum i s term) = bindInferredTerm term withResult
  where
    withResult (AltInferenceResult _ (TypeScheme bvars typ) icons isubst) = Flows.map withVars (Flows.sequence $ fmap (varOrTerm typ) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then Flows.pure $ Left t
          else Right <$> createNewVariable
        withVars vars = AltInferenceResult dummyTerm (TypeScheme (bvars ++ E.rights vars) $ TypeSum (toType <$> vars)) icons isubst
          where
            toType e = case e of
              Left t -> t
              Right v -> TypeVariable v

inferTypeOfTerm :: Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfTerm term = case term of
  TermAnnotated a -> inferTypeOfAnnotatedTerm a
  TermApplication a -> inferTypeOfApplication a
  TermFunction f -> inferTypeOfFunction f
  TermLet l -> inferTypeOfLet l
  TermList els -> inferTypeOfList els
  TermLiteral l -> inferTypeOfLiteral l
  TermMap m -> inferTypeOfMap m
  TermOptional m -> inferTypeOfOptional m
  TermProduct els -> inferTypeOfProduct els
  TermRecord r -> inferTypeOfRecord r
  TermSet s -> inferTypeOfSet s
  TermSum s -> inferTypeOfSum s
  TermTyped t -> inferTypeOfTypedTerm t
  TermUnion i -> inferTypeOfInjection i
  TermVariable name -> inferTypeOfVariable name
  TermWrap w -> inferTypeOfWrappedTerm w

inferTypeOfTupleProjection :: TupleProjection -> Flow AltInferenceContext AltInferenceResult
inferTypeOfTupleProjection (TupleProjection arity idx) = forVars arity withVars
  where
    withVars vars = AltInferenceResult dummyTerm (TypeScheme vars $ Types.function (Types.product types) cod) [] emptyTypeSubst
      where
        types = TypeVariable <$> vars
        cod = types !! idx

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: TypedTerm -> Flow AltInferenceContext AltInferenceResult
inferTypeOfTypedTerm (TypedTerm term _) = inferTypeOfTerm term

inferTypeOfUnwrap :: Name -> Flow AltInferenceContext AltInferenceResult
inferTypeOfUnwrap tname = Flows.bind (requireSchemaType tname) withSchemaType
  where
    withSchemaType (TypeScheme svars styp) = Flows.map withWrappedType (expectWrappedType tname styp)
      where
        withWrappedType wtyp = AltInferenceResult dummyTerm (TypeScheme svars $ Types.function styp wtyp) [] emptyTypeSubst

inferTypeOfVariable :: Name -> Flow AltInferenceContext AltInferenceResult
inferTypeOfVariable var = Flow $ \ctx t -> case M.lookup var (altInferenceContextTypes ctx) of
  Just scheme -> unFlow (Flows.map withoutConstraints $ instantiateTypeScheme scheme) ctx t
  Nothing -> unFlow (Flows.fail $ "Variable not bound to type: " ++ unName var) ctx t

inferTypeOfWrappedTerm :: WrappedTerm -> Flow AltInferenceContext AltInferenceResult
inferTypeOfWrappedTerm (WrappedTerm tname term) = bind2 (requireSchemaType tname) (inferTypeOfTerm term) withResult
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

bindVar :: (Name -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindVar = Flows.bind createNewVariable

bindVar2 :: (Name -> Name -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindVar2 f = bindVar $ \v1 -> bindVar $ \v2 -> f v1 v2

bindVars :: Int -> ([Name] -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindVars n = Flows.bind $ createNewVariables n

bindInferredTerm :: Term -> (AltInferenceResult -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindInferredTerm term = Flows.bind $ inferTypeOfTerm term

bindInferredTerm2 :: Term -> Term -> (AltInferenceResult -> AltInferenceResult -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindInferredTerm2 t1 t2 f = bindInferredTerm t1 $ \r1 -> bindInferredTerm t2 $ f r1

forInferredTerm :: Term -> (AltInferenceResult -> a) -> Flow AltInferenceContext a
forInferredTerm term f = Flows.map f $ inferTypeOfTerm term

forInferredTerm2 :: Term -> Term -> (AltInferenceResult -> AltInferenceResult -> a) -> Flow AltInferenceContext a
forInferredTerm2 t1 t2 f = map2 (inferTypeOfTerm t1) (inferTypeOfTerm t2) f

forInferredTerms :: [Term] -> ([AltInferenceResult] -> a) -> Flow AltInferenceContext a
forInferredTerms terms f = Flows.map f $ Flows.sequence $ inferTypeOfTerm <$> terms

forVar :: (Name -> a) -> Flow AltInferenceContext a
forVar f = Flows.map f createNewVariable

forVars :: Int -> ([Name] -> a) -> Flow AltInferenceContext a
forVars n f = Flows.map f $ createNewVariables n

instantiateTypeScheme :: TypeScheme -> Flow AltInferenceContext TypeScheme
instantiateTypeScheme scheme = Flows.map doSubst (createNewVariables $ L.length oldVars)
    where
      doSubst newVars = TypeScheme newVars $ substituteInType subst $ typeSchemeType scheme
        where
          subst = TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)
      oldVars = L.intersect (L.nub $ typeSchemeVariables scheme) (freeVariablesInSimpleType $ typeSchemeType scheme)

instantiateTypeSchemeAndNormalize :: TypeScheme -> Flow AltInferenceContext TypeScheme
instantiateTypeSchemeAndNormalize scheme = Flows.map normalizeVariablesInTypeScheme (instantiateTypeScheme scheme)

-- Note: does not account for "lambda types"; here we use type schemes instead.
freeVariablesInSimpleType :: Type -> [Name]
freeVariablesInSimpleType = L.nub . foldOverType TraversalOrderPre fold []
  where
    fold rest typ = case typ of
      TypeVariable name -> name : rest
      _ -> rest

mapConstraints :: (TypeSubst -> a) -> [TypeConstraint] -> Flow AltInferenceContext a
mapConstraints f constraints = Flows.map f $ TM.fromEither $ unifyTypeConstraints constraints

maybeToList :: Maybe a -> [a]
maybeToList mx = case mx of
  Just x -> [x]
  Nothing -> []

normalizeVariablesInTypeScheme :: TypeScheme -> TypeScheme
normalizeVariablesInTypeScheme scheme = TypeScheme newVars $ substituteInType subst $ typeSchemeType scheme
  where
    normalVariables = (\n -> Name $ "t" ++ show n) <$> [0..]
    oldVars = typeSchemeVariables scheme
    newVars = L.take (L.length oldVars) normalVariables
    subst =TypeSubst $ M.fromList $ L.zip oldVars (TypeVariable <$> newVars)

createNewVariable :: Flow AltInferenceContext Name
createNewVariable = Flows.map L.head (createNewVariables 1)

createNewVariables :: Int -> Flow AltInferenceContext [Name]
createNewVariables n = Flow helper
  where
    helper ctx t = FlowState value ctx' t
      where
        value = Just ((\n -> Name $ "t" ++ show n) <$> (L.take n [(altInferenceContextVariableCount ctx)..]))
        ctx' = ctx {altInferenceContextVariableCount = n + altInferenceContextVariableCount ctx}

requireSchemaType :: Name -> Flow AltInferenceContext TypeScheme
requireSchemaType tname = do
  cx <- getState
  case M.lookup tname (altInferenceContextTypes cx) of
    Nothing -> Flows.fail $ "No such schema type: " ++ unName tname
    Just ts -> instantiateTypeScheme $ stripTypeSchemeRecursive ts

typeAbstraction :: [Name] -> Term -> Term
typeAbstraction vars body = L.foldl (\b v -> TermTypeAbstraction $ TypeAbstraction v b) body vars

typeApplication :: Term -> [Type] -> Term
typeApplication term types = L.foldl (\t ty -> TermTypeApplication $ TypedTerm t ty) term types

-- | Temporarily add (term variable, type scheme) pairs to the typing environment
withTypeBindings :: [(Name, TypeScheme)] -> Flow AltInferenceContext a -> Flow AltInferenceContext a
withTypeBindings pairs f = Flow helper
  where
    helper ctx0 t0 = FlowState e ctx3 t1
      where
        env = altInferenceContextTypes ctx0
        ctx1 = ctx0 {altInferenceContextTypes = M.union (M.fromList pairs) env}
        FlowState e ctx2 t1 = unFlow f ctx1 t0
        ctx3 = ctx2 {altInferenceContextTypes = env}

unsupported = Flows.fail "Not yet supported"
withoutConstraints scheme = AltInferenceResult dummyTerm scheme [] emptyTypeSubst
