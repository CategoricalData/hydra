module Hydra.Staging.Inference.AltInference where

import Hydra.Variants
import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Mantle
import Hydra.Flows
import Hydra.Strip
import Hydra.Rewriting
import Hydra.Staging.Rewriting
import Hydra.Lib.Flows as Flows
import qualified Hydra.Tools.Monads as TM
import qualified Hydra.Dsl.Types as Types

import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


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

data TypeSubst = TypeSubst { unTypeSubst :: M.Map Name Type }

instance Show TypeSubst where
  show (TypeSubst subst) = "{" ++ L.intercalate ", " (fmap (\((Name k), v) -> k ++ ": " ++ showType v) $ M.toList subst) ++ "}"

composeTypeSubst :: TypeSubst -> TypeSubst -> TypeSubst
composeTypeSubst (TypeSubst firstMap) second@(TypeSubst secondMap) = TypeSubst $
  M.union (substituteInType second <$> firstMap) secondMap

emptyTypeSubst = TypeSubst M.empty

singletonTypeSubst :: Name -> Type -> TypeSubst
singletonTypeSubst v t = TypeSubst $ M.singleton v t

substituteInType :: TypeSubst -> Type -> Type
substituteInType subst = rewriteType rewrite
  where
    rewrite recurse typ = case recurse typ of
      TypeVariable name -> case M.lookup name (unTypeSubst subst) of
        Just styp -> styp
        Nothing -> typ
      t -> t

-- TODO: remove unused bound type variables
substituteInTypeScheme :: TypeSubst -> TypeScheme -> TypeScheme
substituteInTypeScheme subst (TypeScheme vars typ) = TypeScheme vars $ substituteInType subst typ

substituteInConstraint :: TypeSubst -> TypeConstraint -> TypeConstraint
substituteInConstraint subst (TypeConstraint t1 t2 ctx) = TypeConstraint (substituteInType subst t1) (substituteInType subst t2) ctx

substituteInConstraints :: TypeSubst -> [TypeConstraint] -> [TypeConstraint]
substituteInConstraints subst = fmap (substituteInConstraint subst)

--------------------------------------------------------------------------------
-- Inference

data AltInferenceContext
  = AltInferenceContext {
    altInferenceContextLexicon :: M.Map Name TypeScheme,
    altInferenceContextVariableCount :: Int,
    altInferenceContextTypingEnvironment :: M.Map Name TypeScheme}
  deriving (Eq, Ord, Show)

data AltInferenceResult
  = AltInferenceResult {
    altInferenceResultTypeScheme :: TypeScheme,
    altInferenceResultConstraints :: [TypeConstraint]}
  deriving (Eq, Ord)
instance Show AltInferenceResult where
  show (AltInferenceResult scheme constraints) = "{type= " ++ showTypeScheme scheme ++ ", constraints= " ++ show constraints ++ "}"

inferTypeOf :: Term -> Flow AltInferenceContext TypeScheme
inferTypeOf term = Flows.bind (inferTypeOfTerm term) unifyAndSubst
  where
    unifyAndSubst :: AltInferenceResult -> Flow AltInferenceContext TypeScheme
    unifyAndSubst result = Flows.bind (TM.fromEither $ unifyTypeConstraints $ altInferenceResultConstraints result) doSubst
      where
        doSubst :: TypeSubst -> Flow AltInferenceContext TypeScheme
        doSubst subst = instantiateTypeSchemeAndNormalize $ substituteInTypeScheme subst $ altInferenceResultTypeScheme result

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
--  TermRecord ...
  TermSet s -> inferTypeOfSet s
  TermSum s -> inferTypeOfSum s
  TermTyped t -> inferTypeOfTypedTerm t
--  TermUnion ...
  TermVariable name -> inferTypeOfVariable name
  TermWrap w -> inferTypeOfWrappedTerm w

inferTypeOfAnnotatedTerm :: AnnotatedTerm -> Flow AltInferenceContext AltInferenceResult
inferTypeOfAnnotatedTerm (AnnotatedTerm term _) = inferTypeOfTerm term

inferTypeOfApplication :: Application -> Flow AltInferenceContext AltInferenceResult
inferTypeOfApplication (Application lterm rterm) = bindVar2 withVars
  where
    withVars dom cod = map2 (inferTypeOfTerm lterm) (inferTypeOfTerm rterm) withResults
      where
        withResults lresult rresult = AltInferenceResult (TypeScheme tvars $ TypeVariable cod) $ [
            TypeConstraint (Types.function (TypeVariable dom) (TypeVariable cod)) ltyp ctx,
            TypeConstraint (TypeVariable dom) rtyp ctx]
            ++ altInferenceResultConstraints lresult ++ altInferenceResultConstraints rresult
          where
            ctx = Just "application"
            ltyp = typeSchemeType $ altInferenceResultTypeScheme lresult
            rtyp = typeSchemeType $ altInferenceResultTypeScheme rresult
            tvars = typeSchemeVariables (altInferenceResultTypeScheme lresult) ++ typeSchemeVariables (altInferenceResultTypeScheme rresult)

inferTypeOfCollection :: (Type -> Type) -> String -> [Term] -> Flow AltInferenceContext AltInferenceResult
inferTypeOfCollection cons desc els = bindVar withVar
  where
    withVar var = if L.null els
        then Flows.pure $ AltInferenceResult (TypeScheme [var] $ cons $ TypeVariable var) []
        else Flows.map fromResults (Flows.sequence (inferTypeOfTerm <$> els))
      where
        fromResults results = AltInferenceResult (TypeScheme vars $ cons $ TypeVariable var) constraints
          where
            uctx = Just desc
            constraints = cinner ++ couter
            cinner = L.concat (altInferenceResultConstraints <$> results)
            couter = fmap (\t -> TypeConstraint (TypeVariable var) t uctx) types
            types = typeSchemeType . altInferenceResultTypeScheme <$> results
            vars = L.concat (typeSchemeVariables . altInferenceResultTypeScheme <$> results)

inferTypeOfElimination :: Elimination -> Flow AltInferenceContext AltInferenceResult
inferTypeOfElimination elm = case elm of
--  EliminationList fun -> do
--  EliminationOptional (OptionalCases n j) -> do
--  EliminationProduct (TupleProjection arity idx) -> do
--  EliminationRecord (Projection name fname) -> do
--  EliminationUnion (CaseStatement tname def cases) -> do
--  EliminationWrap name -> do
  _ -> Flows.fail $ "Unsupported elimination: " ++ show elm

inferTypeOfFunction :: Function -> Flow AltInferenceContext AltInferenceResult
inferTypeOfFunction f = case f of
  FunctionElimination elm -> inferTypeOfElimination elm
  FunctionLambda l -> inferTypeOfLambda l
  FunctionPrimitive name -> inferTypeOfPrimitive name

inferTypeOfLambda :: Lambda -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLambda (Lambda var _ body) = bindVar withVdom
  where
    withVdom vdom = withTypeBinding var (Types.mono $ TypeVariable vdom)
        $ Flows.map withBodyType (inferTypeOfTerm body)
      where
        -- TODO: prove that vdom will never appear in vars
        withBodyType (AltInferenceResult (TypeScheme vars t) constraints)
          = AltInferenceResult (TypeScheme (vdom:vars) $ Types.function (TypeVariable vdom) t) constraints

-- TODO: propagate rawValueVars and envVars into the final result, possibly after substitution
-- TODO: recursive and mutually recursive let
inferTypeOfLet :: Let -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLet (Let bindings env) = if L.length bindings > 2
    then inferTypeOfTerm $ TermLet (Let [L.head bindings] $ TermLet $ Let (L.tail bindings) env)
    else forSingleBinding $ L.head bindings
  where
    forSingleBinding (LetBinding key value _) = bindVar withVar
      where
        -- Create a temporary type variable for the binding
        withVar var = withTypeBinding key (Types.mono $ TypeVariable var) $
            Flows.bind (inferTypeOfTerm value) withValueType
          where
            -- Unify and substitute over the value constraints
            -- TODO: save the substitution and pass it along, instead of the original set of constraints
            withValueType (AltInferenceResult rawValueScheme valueConstraints) = Flows.bind (TM.fromEither $ unifyTypeConstraints kvConstraints) afterUnification
              where
                rawValueVars = typeSchemeVariables rawValueScheme
                kvConstraints = keyConstraint:valueConstraints
                keyConstraint = TypeConstraint (TypeVariable var) (typeSchemeType rawValueScheme) $ Just "let binding"
                -- Now update the type binding to use the inferred type
                afterUnification subst = withTypeBinding key valueScheme
                    $ Flows.map withEnvType (inferTypeOfTerm env)
                  where
                    valueScheme = substituteInTypeScheme subst rawValueScheme
                    withEnvType (AltInferenceResult envScheme envConstraints) = AltInferenceResult envScheme constraints
                      where
                        constraints = kvConstraints ++ envConstraints
                        envVars = typeSchemeVariables envScheme

inferTypeOfList :: [Term] -> Flow AltInferenceContext AltInferenceResult
inferTypeOfList = inferTypeOfCollection Types.list "list element"

inferTypeOfLiteral :: Literal -> Flow AltInferenceContext AltInferenceResult
inferTypeOfLiteral lit = Flows.pure $ AltInferenceResult (Types.mono $ TypeLiteral $ literalType lit) []

inferTypeOfMap :: M.Map Term Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfMap m = bindVar2 withVars
  where
    withVars kvar vvar = if M.null m
        then Flows.pure $ AltInferenceResult (TypeScheme [kvar, vvar] $ Types.map (TypeVariable kvar) (TypeVariable vvar)) []
        else Flows.map withResults (Flows.sequence $ fmap fromPair $ M.toList m)
      where
        fromPair (k, v) = Flows.bind (inferTypeOfTerm k) withKeyType
          where
            withKeyType (AltInferenceResult (TypeScheme kvars kt) kconstraints) = Flows.map withValueType (inferTypeOfTerm v)
              where
                withValueType (AltInferenceResult (TypeScheme vvars vt) vconstraints)
                  = (kvars ++ vvars,
                     [TypeConstraint (TypeVariable kvar) kt $ Just "map key",
                      TypeConstraint (TypeVariable vvar) vt $ Just "map value"]
                      ++ kconstraints ++ vconstraints)
        withResults pairs = AltInferenceResult (TypeScheme (L.concat (fst <$> pairs)) $ Types.map (TypeVariable kvar) (TypeVariable vvar)) $
          L.concat (snd <$> pairs)

inferTypeOfOptional :: Maybe Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfOptional m = inferTypeOfCollection Types.optional "optional element" $ Y.maybe [] (\e -> [e]) m

inferTypeOfPrimitive :: Name -> Flow AltInferenceContext AltInferenceResult
inferTypeOfPrimitive name = Flow $ \ctx t -> case M.lookup name (altInferenceContextLexicon ctx) of
  Just scheme -> unFlow (Flows.map withoutConstraints $ instantiateTypeScheme scheme) ctx t
  Nothing -> unFlow (Flows.fail $ "No such primitive: " ++ unName name) ctx t

inferTypeOfProduct :: [Term] -> Flow AltInferenceContext AltInferenceResult
inferTypeOfProduct els = if L.null els
    then Flows.pure $ AltInferenceResult (Types.mono $ Types.product []) []
    else Flows.map fromResults (Flows.sequence (inferTypeOfTerm <$> els))
  where
    fromResults results = AltInferenceResult (TypeScheme tvars $ TypeProduct tbodies) constraints
      where
        tvars = L.concat $ typeSchemeVariables . altInferenceResultTypeScheme <$> results
        tbodies = typeSchemeType . altInferenceResultTypeScheme <$> results
        constraints = L.concat $ altInferenceResultConstraints <$> results

inferTypeOfSet :: S.Set Term -> Flow AltInferenceContext AltInferenceResult
inferTypeOfSet = inferTypeOfCollection Types.set "set element" . S.toList

inferTypeOfSum :: Sum -> Flow AltInferenceContext AltInferenceResult
inferTypeOfSum (Sum i s term) = Flows.bind (inferTypeOfTerm term) withResult
  where
    withResult (AltInferenceResult (TypeScheme bvars typ) constraints) = Flows.map withVars (Flows.sequence $ fmap (varOrTerm typ) [0..(s-1)])
      where
        varOrTerm t j = if i == j
          then Flows.pure $ Left t
          else Right <$> createNewVariable
        withVars vars = AltInferenceResult (TypeScheme (bvars ++ E.rights vars) $ TypeSum (toType <$> vars)) constraints
          where
            toType e = case e of
              Left t -> t
              Right v -> TypeVariable v

-- For now, type annotations are simply ignored during inference.
inferTypeOfTypedTerm :: TypedTerm -> Flow AltInferenceContext AltInferenceResult
inferTypeOfTypedTerm (TypedTerm term _) = inferTypeOfTerm term

inferTypeOfVariable :: Name -> Flow AltInferenceContext AltInferenceResult
inferTypeOfVariable var = Flow $ \ctx t -> case M.lookup var (altInferenceContextTypingEnvironment ctx) of
  Just scheme -> unFlow (Flows.map withoutConstraints $ instantiateTypeScheme scheme) ctx t
  Nothing -> unFlow (Flows.fail $ "Variable not bound to type: " ++ unName var) ctx t

inferTypeOfWrappedTerm :: WrappedTerm -> Flow AltInferenceContext AltInferenceResult
inferTypeOfWrappedTerm (WrappedTerm tname term) = Flows.map withResult $ inferTypeOfTerm term
  where
    withResult (AltInferenceResult (TypeScheme vars typ) constraints)
      = AltInferenceResult (TypeScheme vars $ TypeWrap $ WrappedType tname typ) constraints

--------------------------------------------------------------------------------

bindVar :: (Name -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindVar = Flows.bind createNewVariable

bindVar2 :: (Name -> Name -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindVar2 f = bindVar $ \v1 -> bindVar $ \v2 -> f v1 v2

bindVars :: Int -> ([Name] -> Flow AltInferenceContext a) -> Flow AltInferenceContext a
bindVars n = Flows.bind $ createNewVariables n

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

-- | Temporarily add a (term variable, type scheme) to the typing environment
withTypeBinding :: Name -> TypeScheme -> Flow AltInferenceContext a -> Flow AltInferenceContext a
withTypeBinding name scheme f = Flow helper
  where
    helper ctx0 t0 = FlowState e ctx3 t1
      where
        env = altInferenceContextTypingEnvironment ctx0
        ctx1 = ctx0 {altInferenceContextTypingEnvironment = M.insert name scheme env}
        FlowState e ctx2 t1 = unFlow f ctx1 t0
        ctx3 = ctx2 {altInferenceContextTypingEnvironment = env}

unsupported = Flows.fail "Not yet supported"
withoutConstraints scheme = AltInferenceResult scheme []
