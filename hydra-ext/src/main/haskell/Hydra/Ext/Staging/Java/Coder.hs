module Hydra.Ext.Staging.Java.Coder (
  JavaFeatures(..),
  JavaEnvironment(..),
  java8Features,
  moduleToJava,
  encodeTerm,
) where

import Hydra.Kernel
import Hydra.Typing
import Hydra.CoderUtils
import Hydra.Ext.Staging.Java.Utils
import Hydra.Ext.Java.Language
import Hydra.Ext.Staging.Java.Names
import Hydra.Adapt.Modules
import Hydra.Ext.Staging.Java.Serde
import Hydra.Ext.Staging.Java.Settings
import Hydra.Adapt.Utils
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Ext.Java.Syntax as Java
import qualified Hydra.Util as Util

import qualified Control.Monad as CM
import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.List.Split as LS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.String (String)


data JavaSymbolClass =
    JavaSymbolClassConstant
  | JavaSymbolClassNullaryFunction
  -- | A hoisted lambda wrapped in type lambdas. The Int is the number of curried lambda parameters.
  | JavaSymbolClassHoistedLambda Int
  | JavaSymbolClassUnaryFunction
  | JavaSymbolLocalVariable

data JavaEnvironment = JavaEnvironment {
  javaEnvironmentAliases :: Aliases,
  javaEnvironmentTypeContext :: TypeContext
}

data JavaFeatures = JavaFeatures {
  supportsDiamondOperator :: Bool
}

java8Features = JavaFeatures {
  supportsDiamondOperator = False
}

java11Features = JavaFeatures {
  supportsDiamondOperator = True
}

-- For now, the supported features are hard-coded to those of Java 11, rather than being configurable.
javaFeatures = java11Features

-- | New simple adapter version that works with definitions directly
moduleToJava :: Module -> [Definition] -> Flow Graph (M.Map FilePath String)
moduleToJava mod defs = withTrace ("encode module: " ++ unNamespace (moduleNamespace mod)) $ do
    units <- encodeDefinitions mod defs
    return $ M.fromList $ forPair <$> M.toList units
  where
    forPair (name, unit) = (bindingNameToFilePath name, printExpr $ parenthesize $ writeCompilationUnit unit)

addComment :: Java.ClassBodyDeclaration -> FieldType -> Flow Graph Java.ClassBodyDeclarationWithComments
addComment decl field = Java.ClassBodyDeclarationWithComments decl <$> commentsFromFieldType field

analyzeJavaFunction :: JavaEnvironment -> Term -> Flow Graph (FunctionStructure JavaEnvironment)
analyzeJavaFunction env = analyzeFunctionTerm javaEnvironmentTypeContext (\tc e -> e { javaEnvironmentTypeContext = tc }) env

-- | Like analyzeJavaFunction but without type inference for the codomain.
-- This is used when encoding lambdas in contexts where type inference might fail
-- due to hoisting disrupting type annotations.
analyzeJavaFunctionNoInfer :: JavaEnvironment -> Term -> Flow Graph (FunctionStructure JavaEnvironment)
analyzeJavaFunctionNoInfer env = analyzeFunctionTermNoInfer javaEnvironmentTypeContext (\tc e -> e { javaEnvironmentTypeContext = tc }) env

bindingNameToFilePath :: Name -> FilePath
bindingNameToFilePath name = nameToFilePath CaseConventionCamel CaseConventionPascal (FileExtension "java")
    $ unqualifyName $ QualifiedName ns (sanitizeJavaName local)
  where
    QualifiedName ns local = qualifyName name

boundTypeVariables :: Type -> [Name]
boundTypeVariables typ = case typ of
  TypeAnnotated (AnnotatedType typ1 _) -> boundTypeVariables typ1
  TypeForall (ForallType v body) -> v:(boundTypeVariables body)
  _ -> []

-- | Recursively apply a type substitution
applySubstFull :: M.Map Name Type -> Type -> Type
applySubstFull s t = case deannotateType t of
  TypeVariable v -> M.findWithDefault t v s
  TypeFunction (FunctionType d c) ->
    TypeFunction $ FunctionType (applySubstFull s d) (applySubstFull s c)
  TypeApplication (ApplicationType l r) ->
    TypeApplication $ ApplicationType (applySubstFull s l) (applySubstFull s r)
  TypeList t' -> TypeList (applySubstFull s t')
  TypeSet t' -> TypeSet (applySubstFull s t')
  TypeMaybe t' -> TypeMaybe (applySubstFull s t')
  TypeMap (MapType k v) -> TypeMap $ MapType (applySubstFull s k) (applySubstFull s v)
  TypePair (PairType l r) -> TypePair $ PairType (applySubstFull s l) (applySubstFull s r)
  TypeEither (EitherType l r) -> TypeEither $ EitherType (applySubstFull s l) (applySubstFull s r)
  TypeForall (ForallType n b) -> TypeForall $ ForallType n (applySubstFull (M.delete n s) b)
  other -> other

-- | Peel expected argument types from a type scheme body using a substitution
peelExpectedTypes :: M.Map Name Type -> Int -> Type -> [Type]
peelExpectedTypes _ 0 _ = []
peelExpectedTypes subst n t = case deannotateType t of
  TypeFunction (FunctionType d c) -> applySubstFull subst d : peelExpectedTypes subst (n-1) c
  _ -> []

-- | Propagate a correct type annotation through a term. Sets the type annotation on the term
-- and, for lambdas, also recursively annotates the body with the codomain type.
propagateType :: Type -> Term -> Term
propagateType typ term = case deannotateTerm term of
    TermFunction (FunctionLambda (Lambda param mdom body)) ->
      let annotated = setTermAnnotation key_type (Just $ EncodeCore.type_ typ) term
      in case deannotateType typ of
           TypeFunction (FunctionType _ cod) ->
             propagateIntoLambda cod annotated
           _ -> annotated
    TermLet (Let bindings body) ->
      -- For let expressions, the type of the let = type of its body.
      -- Propagate the type into the body.
      setTermAnnotation key_type (Just $ EncodeCore.type_ typ) $
        rebuildLet term bindings (propagateType typ body)
    _ -> setTermAnnotation key_type (Just $ EncodeCore.type_ typ) term
  where
    -- Propagate the codomain type into a lambda's body, traversing through annotations
    propagateIntoLambda cod t = case t of
      TermAnnotated (AnnotatedTerm inner ann) ->
        TermAnnotated $ AnnotatedTerm (propagateIntoLambda cod inner) ann
      TermFunction (FunctionLambda (Lambda param mdom body)) ->
        TermFunction $ FunctionLambda $ Lambda param mdom (propagateType cod body)
      _ -> t
    -- Rebuild a let expression with a new body, preserving annotations
    rebuildLet t bindings newBody = case t of
      TermAnnotated (AnnotatedTerm inner ann) ->
        TermAnnotated $ AnnotatedTerm (rebuildLet inner bindings newBody) ann
      TermLet _ -> TermLet $ Let bindings newBody
      _ -> t

-- | Build a type variable substitution by structurally matching a "fresh" type (from inference
-- annotations) against a "canonical" type (from the type scheme). When both types have a
-- TypeVariable at the same structural position, maps the fresh name to the canonical name.
-- Only includes mappings where the names actually differ.
buildTypeVarSubst :: S.Set Name -> Type -> Type -> M.Map Name Name
buildTypeVarSubst schemeVarSet freshTyp canonTyp = go (deannotateType freshTyp) (deannotateType canonTyp)
  where
    go (TypeVariable fn) (TypeVariable cn)
      | fn /= cn && S.member cn schemeVarSet = M.singleton fn cn
    go (TypeFunction (FunctionType fd fc)) (TypeFunction (FunctionType cd cc)) =
      M.union (go (deannotateType fd) (deannotateType cd)) (go (deannotateType fc) (deannotateType cc))
    go (TypeApplication (ApplicationType fl fr)) (TypeApplication (ApplicationType cl cr)) =
      M.union (go (deannotateType fl) (deannotateType cl)) (go (deannotateType fr) (deannotateType cr))
    go (TypeList ft) (TypeList ct) = go (deannotateType ft) (deannotateType ct)
    go (TypeSet ft) (TypeSet ct) = go (deannotateType ft) (deannotateType ct)
    go (TypeMaybe ft) (TypeMaybe ct) = go (deannotateType ft) (deannotateType ct)
    go (TypeMap (MapType fk fv)) (TypeMap (MapType ck cv)) =
      M.union (go (deannotateType fk) (deannotateType ck)) (go (deannotateType fv) (deannotateType cv))
    go (TypePair (PairType fl fr)) (TypePair (PairType cl cr)) =
      M.union (go (deannotateType fl) (deannotateType cl)) (go (deannotateType fr) (deannotateType cr))
    go (TypeEither (EitherType fl fr)) (TypeEither (EitherType cl cr)) =
      M.union (go (deannotateType fl) (deannotateType cl)) (go (deannotateType fr) (deannotateType cr))
    go (TypeForall (ForallType _ fb)) (TypeForall (ForallType _ cb)) =
      go (deannotateType fb) (deannotateType cb)
    go (TypeForall (ForallType _ fb)) ct = go (deannotateType fb) ct
    go ft (TypeForall (ForallType _ cb)) = go ft (deannotateType cb)
    go _ _ = M.empty

-- | Build a mapping from scheme type variables to actual types by structurally matching
-- a scheme type against an actual type. Only maps variables that are in the schemeVarSet.
buildTypeSubst :: S.Set Name -> Type -> Type -> M.Map Name Type
buildTypeSubst schemeVarSet schemeType actualType = go (deannotateType schemeType) (deannotateType actualType)
  where
    go (TypeVariable v) actual | S.member v schemeVarSet = M.singleton v actual
    go (TypeFunction (FunctionType sd sc)) (TypeFunction (FunctionType ad ac)) =
      M.union (go (deannotateType sd) (deannotateType ad)) (go (deannotateType sc) (deannotateType ac))
    go (TypeApplication (ApplicationType sl sr)) (TypeApplication (ApplicationType al ar)) =
      M.union (go (deannotateType sl) (deannotateType al)) (go (deannotateType sr) (deannotateType ar))
    go (TypeList st) (TypeList at') = go (deannotateType st) (deannotateType at')
    go (TypeSet st) (TypeSet at') = go (deannotateType st) (deannotateType at')
    go (TypeMaybe st) (TypeMaybe at') = go (deannotateType st) (deannotateType at')
    go (TypeMap (MapType sk sv)) (TypeMap (MapType ak av)) =
      M.union (go (deannotateType sk) (deannotateType ak)) (go (deannotateType sv) (deannotateType av))
    go (TypePair (PairType sl sr)) (TypePair (PairType al ar)) =
      M.union (go (deannotateType sl) (deannotateType al)) (go (deannotateType sr) (deannotateType ar))
    go (TypeEither (EitherType sl sr)) (TypeEither (EitherType al ar)) =
      M.union (go (deannotateType sl) (deannotateType al)) (go (deannotateType sr) (deannotateType ar))
    go (TypeForall (ForallType _ sb)) at' = go (deannotateType sb) at'
    go st (TypeForall (ForallType _ ab)) = go st (deannotateType ab)
    go _ _ = M.empty

-- | Compute corrected type applications for a function call by looking up the callee's
-- type scheme and building a substitution from argument types. The IR's type applications
-- can be in the wrong order due to normalizeTypeVariablesInTerm renumbering.
-- The approach: build a mapping from scheme type variables to actual types using argument
-- type matching, then fill in remaining variables from the IR's type apps.
correctTypeApps :: TypeContext -> Name -> [Term] -> [Type] -> Flow Graph [Type]
correctTypeApps tc name args fallbackTypeApps = do
    mel <- dereferenceElement name
    case mel of
      Nothing -> return fallbackTypeApps
      Just el -> case bindingType el of
        Nothing -> return fallbackTypeApps
        Just ts -> do
          let schemeType = typeSchemeType ts
              -- Filter scheme vars to only those used in the scheme type,
              -- matching the filtering done in encodeTermDefinition.
              allSchemeVars = L.filter (\(Name n) -> not ('.' `elem` n)) $ typeSchemeVariables ts
              schemeTypeVars = collectTypeVars schemeType
              usedFlags = fmap (\v -> S.member v schemeTypeVars) allSchemeVars
              usedSchemeVars = [v | (v, True) <- L.zip allSchemeVars usedFlags]
              -- Detect accumulator unification (matching encodeTermDefinition)
              countParams t = case deannotateType t of
                TypeFunction (FunctionType _ c) -> 1 + countParams c
                _ -> 0
              nParams = countParams schemeType
              peelDoms2 0 t = ([], t)
              peelDoms2 n t = case deannotateType t of
                TypeFunction (FunctionType d c) -> let (ds, r) = peelDoms2 (n-1) c in (d:ds, r)
                _ -> ([], t)
              (calleeDoms, calleeCod) = peelDoms2 nParams schemeType
              overgenSubst = detectAccumulatorUnification calleeDoms calleeCod usedSchemeVars
              -- Apply both phantom and overgen filtering
              keepFlags = fmap (\v -> S.member v schemeTypeVars && not (M.member v overgenSubst)) allSchemeVars
              schemeVars = [v | (v, True) <- L.zip allSchemeVars keepFlags]
              -- Also filter the fallback type args to match
              filteredFallback0 = if L.length allSchemeVars == L.length fallbackTypeApps
                then [t | (t, True) <- L.zip fallbackTypeApps keepFlags]
                else fallbackTypeApps
              -- Apply overgen substitution to the filtered type args
              filteredFallback = if M.null overgenSubst then filteredFallback0
                else fmap (substituteTypeVarsWithTypes overgenSubst) filteredFallback0
          if L.null schemeVars || L.length schemeVars /= L.length filteredFallback
            then return filteredFallback
            else do
              let fallbackTypeApps = filteredFallback
              let schemeVarSet = S.fromList schemeVars
                  -- Build the substitution that the IR's type apps imply
                  irSubst = M.fromList $ L.zip schemeVars fallbackTypeApps
                  -- Peel domain types from the scheme type
                  peelDoms 0 t = ([], t)
                  peelDoms n t = case deannotateType t of
                    TypeFunction (FunctionType d c) -> let (ds, r) = peelDoms (n-1) c in (d:ds, r)
                    _ -> ([], t)
                  (schemeDoms, _schemeCod) = peelDoms (L.length args) schemeType
              -- For each argument, try to get its type from annotations (cheap).
              -- If annotation is not available, trust the IR order. This avoids expensive
              -- type inference that can cause slowdowns in interpreted mode (GHCi).
              mArgTypes <- CM.mapM (\arg -> getType (termAnnotationInternal arg)) args
              if not (L.all Y.isJust mArgTypes)
                then return fallbackTypeApps  -- Can't verify without inference; trust the IR
                else do
                  let argTypes = Y.catMaybes mArgTypes
                  -- Apply the IR substitution to each scheme domain type and check consistency
                  let applySubst subst t = case deannotateType t of
                        TypeVariable v -> M.findWithDefault t v subst
                        _ -> t  -- Only substitute top-level type variables for simplicity
                  let irDoms = fmap (applySubst irSubst) schemeDoms
                  -- Check if the IR domains match the argument types
                  let domsMatch = L.all (\(irDom, argType) ->
                        typesMatch (deannotateType irDom) (deannotateType argType)) (L.zip irDoms argTypes)
                  if domsMatch
                    then return fallbackTypeApps  -- IR is correct, no need to fix
                    else do
                      -- IR is wrong; try to build correct mapping from argument types directly.
                      let argSubst = M.fromList $ concatMap (\(sdom, argType) ->
                            case deannotateType sdom of
                              TypeVariable v | S.member v schemeVarSet -> [(v, argType)]
                              _ -> []
                            ) (L.zip schemeDoms argTypes)
                      let resolvedVars = M.keysSet argSubst
                          unresolvedVars = L.filter (\v -> not $ S.member v resolvedVars) schemeVars
                          usedTypes = S.fromList $ M.elems argSubst
                          unusedIrTypes = L.filter (\t -> not $ S.member t usedTypes) fallbackTypeApps
                          remainingSubst = M.fromList $ L.zip unresolvedVars unusedIrTypes
                          fullSubst = M.union argSubst remainingSubst
                      let result = fmap (\v -> M.findWithDefault (TypeVariable v) v fullSubst) schemeVars
                      return result
  where
    typesMatch (TypeVariable a) (TypeVariable b) = a == b
    typesMatch (TypeWrap a) (TypeWrap b) = wrappedTypeTypeName a == wrappedTypeTypeName b
    typesMatch _ _ = True  -- Allow structural matches to pass

-- | Build a type variable substitution by walking a term and comparing lambda domain types
-- (which are normalized by normalizeTypeVariablesInTerm) against the annotation map types
-- (which are NOT normalized). This recovers the fresh→canonical mapping.
buildSubstFromAnnotations :: S.Set Name -> Term -> Flow Graph (M.Map Name Name)
buildSubstFromAnnotations schemeVarSet term = do
    g <- getState
    return $ go g term
  where
    go g term = case term of
      TermAnnotated (AnnotatedTerm body anns) ->
        let bodySubst = go g body
            annSubst = case M.lookup key_type anns of
              Nothing -> M.empty
              Just typeTerm -> case DecodeCore.type_ g typeTerm of
                Left _ -> M.empty
                Right annType -> case deannotateTerm body of
                  TermFunction (FunctionLambda (Lambda _ (Just dom) _)) ->
                    -- Match the annotation's function type domain against the lambda's normalized domain
                    case deannotateType annType of
                      TypeFunction (FunctionType annDom _) ->
                        buildTypeVarSubst schemeVarSet annDom dom
                      _ -> M.empty
                  _ -> M.empty
        in M.union annSubst bodySubst
      TermApplication (Application lhs rhs) -> M.union (go g lhs) (go g rhs)
      TermFunction (FunctionLambda (Lambda _ _ body)) -> go g body
      TermFunction (FunctionElimination (EliminationUnion (CaseStatement _ mdef cases))) ->
        let defSubst = maybe M.empty (go g) mdef
            caseSubsts = M.unions $ fmap (go g . fieldTerm) cases
        in M.union defSubst caseSubsts
      TermLet (Let bindings body) ->
        M.union (M.unions $ fmap (go g . bindingTerm) bindings) (go g body)
      TermList terms -> M.unions $ fmap (go g) terms
      TermMaybe (Just t) -> go g t
      TermPair (a, b) -> M.union (go g a) (go g b)
      TermRecord (Record _ fields) -> M.unions $ fmap (go g . fieldTerm) fields
      TermSet terms -> M.unions $ fmap (go g) $ S.toList terms
      TermTypeApplication (TypeApplicationTerm body _) -> go g body
      TermTypeLambda (TypeLambda _ body) -> go g body
      TermEither (Left t) -> go g t
      TermEither (Right t) -> go g t
      _ -> M.empty

-- | Collect all type variable names from a type
collectTypeVars :: Type -> S.Set Name
collectTypeVars typ = go (deannotateType typ)
  where
    go t = case t of
      TypeVariable name -> S.singleton name
      TypeFunction (FunctionType d c) -> S.union (go $ deannotateType d) (go $ deannotateType c)
      TypeApplication (ApplicationType l r) -> S.union (go $ deannotateType l) (go $ deannotateType r)
      TypeList t' -> go (deannotateType t')
      TypeSet t' -> go (deannotateType t')
      TypeMaybe t' -> go (deannotateType t')
      TypeMap (MapType k v) -> S.union (go $ deannotateType k) (go $ deannotateType v)
      TypePair (PairType l r) -> S.union (go $ deannotateType l) (go $ deannotateType r)
      TypeEither (EitherType l r) -> S.union (go $ deannotateType l) (go $ deannotateType r)
      TypeForall (ForallType _ b) -> go (deannotateType b)
      _ -> S.empty

-- | Substitute type variables with types (more general than substituteTypeVariables which maps Name->Name).
substituteTypeVarsWithTypes :: M.Map Name Type -> Type -> Type
substituteTypeVarsWithTypes subst = rewrite
  where
    rewrite t = case deannotateType t of
      TypeVariable v -> case M.lookup v subst of
        Just replacement -> replacement
        Nothing -> t
      TypeFunction (FunctionType d c) -> TypeFunction (FunctionType (rewrite d) (rewrite c))
      TypePair (PairType a b) -> TypePair (PairType (rewrite a) (rewrite b))
      TypeApplication (ApplicationType f a) -> TypeApplication (ApplicationType (rewrite f) (rewrite a))
      TypeList inner -> TypeList (rewrite inner)
      TypeSet inner -> TypeSet (rewrite inner)
      TypeMaybe inner -> TypeMaybe (rewrite inner)
      TypeMap (MapType k v) -> TypeMap (MapType (rewrite k) (rewrite v))
      TypeEither (EitherType l r) -> TypeEither (EitherType (rewrite l) (rewrite r))
      TypeForall (ForallType param body) -> TypeForall (ForallType param (rewrite body))
      _ -> t

-- | Apply a type substitution to all type annotations in a term.
-- This walks the term structure and, for each TermAnnotated node,
-- replaces the "type" annotation (if present) with the substituted version.
-- Also updates lambda domains and type applications.
applyOvergenSubstToTermAnnotations :: M.Map Name Type -> Term -> Flow Graph Term
applyOvergenSubstToTermAnnotations subst term0 = do
    cx <- Monads.getState
    return $ go cx term0
  where
    go cx term = case term of
      TermAnnotated (AnnotatedTerm inner ann) ->
        let ann' = case M.lookup key_type ann of
              Just typeTerm -> case DecodeCore.type_ cx typeTerm of
                Right t ->
                  let t' = substituteTypeVarsWithTypes subst t
                  in M.insert key_type (EncodeCore.type_ t') ann
                Left _ -> ann  -- couldn't decode, leave as-is
              Nothing -> ann
        in TermAnnotated (AnnotatedTerm (go cx inner) ann')
      TermApplication (Application lhs rhs) ->
        TermApplication (Application (go cx lhs) (go cx rhs))
      TermFunction (FunctionLambda (Lambda param mdom body)) ->
        TermFunction (FunctionLambda (Lambda param (fmap (substituteTypeVarsWithTypes subst) mdom) (go cx body)))
      TermFunction (FunctionElimination elm) -> case elm of
        EliminationUnion cs -> TermFunction (FunctionElimination (EliminationUnion (CaseStatement
          (caseStatementTypeName cs)
          (fmap (go cx) (caseStatementDefault cs))
          (fmap (\f -> Field (fieldName f) (go cx (fieldTerm f))) (caseStatementCases cs)))))
        _ -> term
      TermLet (Let bindings body) ->
        TermLet (Let (fmap (\b -> Binding (bindingName b) (go cx (bindingTerm b)) (bindingType b)) bindings) (go cx body))
      TermTypeApplication (TypeApplicationTerm body typ) ->
        TermTypeApplication (TypeApplicationTerm (go cx body) (substituteTypeVarsWithTypes subst typ))
      TermTypeLambda (TypeLambda param body) ->
        TermTypeLambda (TypeLambda param (go cx body))
      _ -> term

-- | Detect over-generalized type variables in a scheme type.
-- After hoisting, inference may create separate type variables for what should be the
-- same type. This function detects several patterns:
-- 1. Accumulator fold: param1 returns (V,...), param2 returns (W,...) with same input V
-- 2. Context threading: param1 is V->...->V, param2 is V->...->W (W should = V)
-- 3. Dangling codomain: type var in codomain not in any domain
-- Returns a substitution mapping redundant vars to their canonical equivalents.
detectAccumulatorUnification :: [Type] -> Type -> [Name] -> M.Map Name Type
detectAccumulatorUnification doms cod tparams =
    let -- Extract input/output type variable pairs from function-typed parameters.
        -- For a param like (A -> ... -> (B, ...)), extract (A, B) where B is
        -- the first element of a pair return type.
        extractInOutPair :: Type -> [(Name, Name)]
        extractInOutPair t = case deannotateType t of
          TypeFunction (FunctionType d c) ->
            case deannotateType d of
              TypeVariable inVar ->
                let retType = unwrapReturnType c
                in case deannotateType retType of
                  -- Pair first element: V -> ... -> (W, ...)
                  TypePair (PairType first _) -> case deannotateType first of
                    TypeVariable outVar -> [(inVar, outVar)]
                    _ -> []
                  _ -> []
              _ -> []
          _ -> []
        -- Extract input/output pairs for direct variable returns in
        -- "context extension" functions: functions of shape ... -> V -> X -> V (or W)
        -- where V is a type variable, X is a concrete type, and the return is V or W.
        -- Also handles leading non-variable arguments by skipping them.
        -- Only matches when there's exactly one concrete-typed argument between the
        -- variable input and the variable return (the "extension target" type).
        tparamSet = S.fromList tparams
        extractDirectReturn :: Type -> [(Name, Name)]
        extractDirectReturn t = go t
          where
            go t = case deannotateType t of
              TypeFunction (FunctionType d c) ->
                case deannotateType d of
                  TypeVariable inVar | S.member inVar tparamSet ->
                    -- After the type-param input, expect: X -> V
                    -- where X is NOT a type parameter (could be a concrete named type)
                    -- and V is a type parameter variable.
                    case deannotateType c of
                      TypeFunction (FunctionType midArg retPart) ->
                        case deannotateType midArg of
                          TypeVariable midVar | S.member midVar tparamSet -> []  -- middle is a type param, not a concrete type
                          _ -> case deannotateType retPart of
                            TypeVariable outVar | S.member outVar tparamSet -> [(inVar, outVar)]
                            _ -> []
                      _ -> []
                  -- Skip non-type-param leading domains and continue
                  _ -> go c
              _ -> []
        -- Unwrap nested function types to get the final return type
        unwrapReturnType t = case deannotateType t of
          TypeFunction (FunctionType _ c) -> unwrapReturnType c
          -- Also look through Flow/monadic wrappers
          TypeApplication (ApplicationType _ inner) -> unwrapReturnType inner
          _ -> t
        -- Helper: convert Name->Name map to Name->Type map (wrapping values as TypeVariable)
        toTypeMap :: M.Map Name Name -> M.Map Name Type
        toTypeMap = M.map TypeVariable
        -- Collect all input/output pairs (pair-based)
        allPairs = concatMap extractInOutPair doms
        -- Group by input variable: for each input var, collect all output vars
        groupedByInput = L.foldl' (\m (inv, outv) ->
          M.insertWith (++) inv [outv] m) M.empty allPairs
        -- For each group, if there's a "self-referencing" pair (in == out) and other
        -- pairs with different output vars, substitute those other vars to the self-ref var
        selfRefSubst = M.foldlWithKey' (\subst inVar outVars ->
          if inVar `elem` outVars
            then L.foldl' (\s v -> if v /= inVar then M.insert v inVar s else s) subst outVars
            else subst) M.empty groupedByInput
        -- Direct return self-ref detection (for context-threading patterns like Hoisting).
        -- Only unify V -> ... -> W to V when:
        -- 1. There are at least 2 self-refs (V -> ... -> V) for the same input var
        -- 2. The non-self var W doesn't appear in any domain type as an input var
        --    (i.e., W is only used as a return type, not as an input to any function param)
        -- This prevents false positives where W is genuinely a different type.
        directPairs = concatMap extractDirectReturn doms
        groupedDirect = L.foldl' (\m (inv, outv) ->
          M.insertWith (++) inv [outv] m) M.empty directPairs
        -- Collect all input vars from direct return patterns
        directInputVars = S.fromList $ fmap fst directPairs
        -- Also protect the codomain variable: if the method returns V directly,
        -- don't substitute V (it's the result type, not a context type).
        codVar = case deannotateType cod of
          TypeVariable v -> Just v
          _ -> Nothing
        directRefSubst = M.foldlWithKey' (\subst inVar outVars ->
          let selfRefCount = L.length $ L.filter (== inVar) outVars
              nonSelfVars = L.filter (/= inVar) outVars
              -- Only substitute vars that:
              -- 1. Don't appear as input vars in other patterns
              -- 2. Are not the codomain variable (the function's return type)
              safeNonSelfVars = L.filter (\v ->
                not (S.member v directInputVars) &&
                codVar /= Just v) nonSelfVars
          in if selfRefCount >= 2 && not (L.null safeNonSelfVars)
            then L.foldl' (\s v -> M.insert v inVar s) subst safeNonSelfVars
            else subst) M.empty groupedDirect
        -- Extract the type variable from the first element of a pair type
        findPairFirst t = case deannotateType t of
          TypePair (PairType first _) -> case deannotateType first of
            TypeVariable v -> Just v
            _ -> Nothing
          _ -> Nothing
        -- Check the codomain for vars that should be unified
        codSubst = case findPairFirst cod of
          Just codVar | not (M.member codVar selfRefSubst) ->
            let selfRefVars = [inVar | (inVar, outVars) <- M.toList groupedByInput, inVar `elem` outVars]
            in case selfRefVars of
              (refVar:_) | codVar /= refVar -> M.singleton codVar refVar
              _ -> M.empty
          _ -> M.empty
        -- Dangling codomain vars: type vars in codomain but not in any domain
        domVars = S.unions $ fmap collectTypeVars doms
        danglingSubst = case findPairFirst cod of
          Just codVar | not (S.member codVar domVars) ->
            let selfRefVars = [inVar | (inVar, outVars) <- M.toList groupedByInput, inVar `elem` outVars]
            in case selfRefVars of
              (refVar:_) -> M.singleton codVar (TypeVariable refVar)
              [] -> M.empty
          _ -> M.empty
    in M.unions [toTypeMap selfRefSubst, toTypeMap codSubst, danglingSubst,
                 toTypeMap directRefSubst]

-- | Filter type arguments to remove those at positions corresponding to phantom
-- scheme variables (variables in the scheme's variable list that don't appear in
-- the scheme's actual type). This keeps call-site type args consistent with the
-- callee's declaration, which has already been filtered by encodeTermDefinition.
filterPhantomTypeArgs :: Name -> [Type] -> Flow Graph [Type]
filterPhantomTypeArgs calleeName allTypeArgs = do
    mel <- dereferenceElement calleeName
    case mel of
      Nothing -> return allTypeArgs
      Just el -> case bindingType el of
        Nothing -> return allTypeArgs
        Just ts -> do
          let isSimpleName (Name n) = not ('.' `elem` n)
              schemeVars = L.filter isSimpleName $ typeSchemeVariables ts
              schemeTypeVars = collectTypeVars (typeSchemeType ts)
              -- Filter phantom vars (not used in scheme type)
              usedFlags = fmap (\v -> S.member v schemeTypeVars) schemeVars
              -- Also detect accumulator unification for the callee's scheme
              schemeType = typeSchemeType ts
              numSchemeVars = L.length schemeVars
              peelDoms n t
                | n <= 0 = ([], t)
                | otherwise = case deannotateType t of
                    TypeFunction (FunctionType d c) ->
                      let (ds, cod) = peelDoms (n - 1) c in (d:ds, cod)
                    _ -> ([], t)
              -- Count parameters by looking at the scheme type's function structure
              countParams t = case deannotateType t of
                TypeFunction (FunctionType _ c) -> 1 + countParams c
                _ -> 0
              nParams = countParams schemeType
              (calleeDoms, calleeCod) = peelDoms nParams schemeType
              overgenSubst = detectAccumulatorUnification calleeDoms calleeCod schemeVars
              -- A var should be kept if: used in scheme AND not substituted away by overgen
              keepFlags = fmap (\v -> S.member v schemeTypeVars && not (M.member v overgenSubst)) schemeVars
          if L.length schemeVars /= L.length allTypeArgs
            then return allTypeArgs  -- Length mismatch; don't filter
            else do
              -- Apply the overgen substitution to the kept type args
              let filtered = [t | (t, keep) <- L.zip allTypeArgs keepFlags, keep]
              if not (M.null overgenSubst)
                then return $ fmap (substituteTypeVarsWithTypes overgenSubst) filtered
                else return filtered

-- | Apply a lambda cast if the type is safe (doesn't contain potentially wrong type variables).
-- A cast is safe if all type variables that would be rendered as Java type variables are
-- "trusted" — they appear in the enclosing method's parameter types. Type variables from
-- inference annotations that don't match trusted vars might be wrong (e.g., t10 from a
-- fresh instantiation being rendered as T10 instead of the correct T0).
applyCastIfSafe :: Aliases -> Type -> Java.Expression -> Flow Graph Java.Expression
applyCastIfSafe aliases castType expr = do
  let trusted = aliasesTrustedTypeVars aliases
      inScope = aliasesInScopeTypeParams aliases
      -- Collect type variables in the cast type that would be rendered as Java type variables
      castVars = collectTypeVars castType
      javaTypeVars = S.filter (\v -> S.member v inScope || isLambdaBoundVariable v) castVars
      -- A cast is safe if all Java type variables are trusted (appear in method params)
      -- or if there are no trusted vars to check against (e.g., top-level context)
      isSafe = S.null trusted || S.null javaTypeVars || S.isSubsetOf javaTypeVars trusted
  if isSafe
    then do
      jtype <- encodeType aliases S.empty castType
      rt <- javaTypeToJavaReferenceType jtype
      return $ javaCastExpressionToJavaExpression $
        javaCastExpression rt (javaExpressionToJavaUnaryExpression expr)
    else return expr  -- Skip cast: type vars might be wrong

-- | Check if a type contains any type variables that would be rendered as Java type variables
-- (i.e., they are in scope or pass isLambdaBoundVariable). Used to decide whether a lambda
-- | Try to infer the function type from the function structure when type annotations are unavailable.
-- For lambdas with domain annotations, we can extract the domain type.
-- The codomain is inferred from the body's type annotation if available.
tryInferFunctionType :: Function -> Maybe Type
tryInferFunctionType fun = case fun of
  FunctionLambda (Lambda _ mdom body) -> do
    -- If lambda has a domain type annotation, use it
    dom <- mdom
    -- Try to get the body's type from its annotation
    cod <- case body of
      TermAnnotated (AnnotatedTerm _ ann) -> do
        typeTerm <- M.lookup key_type ann
        -- The type term should encode a Type; decode it
        decodeTypeFromTerm typeTerm
      _ -> Nothing
    Just $ TypeFunction $ FunctionType dom cod
  _ -> Nothing
  where
    -- Decode a Type from its term encoding
    decodeTypeFromTerm :: Term -> Maybe Type
    decodeTypeFromTerm term = case deannotateTerm term of
      TermUnion (Injection tname (Field fname fterm)) ->
        if tname == Name "hydra.core.Type"
          then case unName fname of
            "annotated" -> case fterm of
              TermRecord (Record _ fields) -> do
                bodyField <- L.find (\f -> fieldName f == Name "body") fields
                decodeTypeFromTerm (fieldTerm bodyField)
              _ -> Nothing
            "application" -> case fterm of
              TermRecord (Record _ fields) -> do
                funcField <- L.find (\f -> fieldName f == Name "function") fields
                argField <- L.find (\f -> fieldName f == Name "argument") fields
                func <- decodeTypeFromTerm (fieldTerm funcField)
                arg <- decodeTypeFromTerm (fieldTerm argField)
                Just $ TypeApplication $ ApplicationType func arg
              _ -> Nothing
            "function" -> case fterm of
              TermRecord (Record _ fields) -> do
                domField <- L.find (\f -> fieldName f == Name "domain") fields
                codField <- L.find (\f -> fieldName f == Name "codomain") fields
                dom <- decodeTypeFromTerm (fieldTerm domField)
                cod <- decodeTypeFromTerm (fieldTerm codField)
                Just $ TypeFunction $ FunctionType dom cod
              _ -> Nothing
            "variable" -> case fterm of
              TermWrap (WrappedTerm _ (TermLiteral (LiteralString s))) -> Just $ TypeVariable $ Name s
              _ -> Nothing
            "literal" -> case fterm of
              TermUnion (Injection _ (Field ltName _)) ->
                if unName ltName == "string"
                  then Just $ TypeLiteral LiteralTypeString
                  else Nothing  -- Simplified, could handle more
              _ -> Nothing
            _ -> Nothing  -- Other type variants not handled yet
          else Nothing
      _ -> Nothing

classModsPublic :: [Java.ClassModifier]
classModsPublic = [Java.ClassModifierPublic]

classifyDataReference :: Name -> Flow Graph JavaSymbolClass
classifyDataReference name = do
  mel <- dereferenceElement name
  case mel of
    Nothing -> return JavaSymbolLocalVariable
    Just el -> do
      g <- getState
      tc <- graphToTypeContext g
      ts <- case bindingType el of
        Nothing -> fail $ "no type scheme for element " ++ unName (bindingName el)
        Just ts -> pure ts
      return $ classifyDataTerm ts $ bindingTerm el

classifyDataTerm :: TypeScheme -> Term -> JavaSymbolClass
classifyDataTerm ts term = if isLambda term
    then case countLambdaParams term of
      n | n > 1 -> JavaSymbolClassHoistedLambda n
      _ -> JavaSymbolClassUnaryFunction
    else if hasTypeParameters
      then case countLambdaParams (stripTypeLambdas term) of
        n | n > 0 -> JavaSymbolClassHoistedLambda n
        _ -> JavaSymbolClassNullaryFunction
      -- All non-lambda, non-polymorphic terms are nullary functions (not constants)
      -- to avoid forward-reference NPEs in interface static field initialization order.
      else JavaSymbolClassNullaryFunction
  where
    hasTypeParameters = not $ L.null $ typeSchemeVariables ts
    isUnsupportedVariant = case deannotateTerm term of
      TermLet _ -> True
      _ -> False
    stripTypeLambdas t = case deannotateTerm t of
      TermTypeLambda (TypeLambda _ body) -> stripTypeLambdas body
      _ -> t
    countLambdaParams t = case deannotateTerm t of
      TermFunction (FunctionLambda (Lambda _ _ body)) -> 1 + countLambdaParams body
      TermLet (Let _ body) -> countLambdaParams body
      _ -> 0

constantDecl :: String -> Aliases -> Name -> Flow Graph Java.ClassBodyDeclarationWithComments
constantDecl javaName aliases name = do
  g <- getState
  tc <- graphToTypeContext g
  let env = JavaEnvironment aliases tc
  jt <- encodeType aliases S.empty $ TypeVariable _Name
  arg <- encodeTerm env $ Terms.string $ unName name
  let init = Java.VariableInitializerExpression $ javaConstructorCall (javaConstructorName nameName Nothing) [arg] Nothing
  let var = javaVariableDeclarator (Java.Identifier javaName) (Just init)
  return $ noComment $ javaMemberField mods jt var
  where
    mods = [Java.FieldModifierPublic, Java.FieldModifierStatic, Java.FieldModifierFinal]
    nameName = nameToJavaName aliases _Name

constantDeclForFieldType :: Aliases -> FieldType -> Flow Graph Java.ClassBodyDeclarationWithComments
constantDeclForFieldType aliases ftyp = constantDecl javaName aliases name
  where
    name = fieldTypeName ftyp
    javaName = "FIELD_NAME_" ++ nonAlnumToUnderscores (convertCase CaseConventionCamel CaseConventionUpperSnake $ unName name)

constantDeclForTypeName :: Aliases -> Name -> Flow Graph Java.ClassBodyDeclarationWithComments
constantDeclForTypeName = constantDecl "TYPE_NAME"

constructElementsInterface :: Module -> [Java.InterfaceMemberDeclaration] -> (Name, Java.CompilationUnit)
constructElementsInterface mod members = (elName, cu)
  where
    cu = Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) [] [decl]
    pkg = javaPackageDeclaration $ moduleNamespace mod
    mods = [Java.InterfaceModifierPublic]
    className = elementsClassName $ moduleNamespace mod
    elName = unqualifyName $ QualifiedName (Just $ moduleNamespace mod) className
    allMembers = members
    body = Java.InterfaceBody allMembers
    itf = Java.TypeDeclarationInterface $ Java.InterfaceDeclarationNormalInterface $
      Java.NormalInterfaceDeclaration mods (javaTypeIdentifier className) [] [] body
    decl = Java.TypeDeclarationWithComments itf $ moduleDescription mod

-- | Split a constant declaration into a field + helper method to avoid large <clinit>.
--   Transforms: Type x = <expr>;
--   Into:       Type x = _init_x();
--               static Type _init_x() { return <expr>; }
splitConstantInitializer :: Java.InterfaceMemberDeclaration -> [Java.InterfaceMemberDeclaration]
splitConstantInitializer member = case member of
  Java.InterfaceMemberDeclarationConstant (Java.ConstantDeclaration mods utype vars) ->
    L.concatMap (splitVar mods utype) vars
  _ -> [member]
  where
    splitVar mods utype (Java.VariableDeclarator vid (Just (Java.VariableInitializerExpression expr))) =
      [field, helper]
      where
        varName = javaIdentifierToString $ Java.variableDeclaratorIdIdentifier vid
        helperName = "_init_" ++ varName
        callExpr = javaMethodInvocationToJavaExpression $
          methodInvocation Nothing (Java.Identifier helperName) []
        field = Java.InterfaceMemberDeclarationConstant $
          Java.ConstantDeclaration mods utype
            [Java.VariableDeclarator vid (Just $ Java.VariableInitializerExpression callExpr)]
        returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just expr
        resultType = Java.ResultType utype
        helper = interfaceMethodDeclaration
          [Java.InterfaceMethodModifierStatic, Java.InterfaceMethodModifierPrivate]
          [] helperName [] resultType (Just [returnSt])
    splitVar mods utype var = [Java.InterfaceMemberDeclarationConstant $
      Java.ConstantDeclaration mods utype [var]]

javaIdentifierToString :: Java.Identifier -> String
javaIdentifierToString (Java.Identifier s) = s

declarationForRecordType :: Bool -> Bool -> Aliases -> [Java.TypeParameter] -> Name
  -> [FieldType] -> Flow Graph Java.ClassDeclaration
declarationForRecordType isInner isSer aliases tparams elName fields = declarationForRecordType' isInner isSer aliases tparams elName Nothing fields

-- | Extended version that accepts an optional parent name for union variant compareTo generation.
declarationForRecordType' :: Bool -> Bool -> Aliases -> [Java.TypeParameter] -> Name -> Maybe Name -> [FieldType]
  -> Flow Graph Java.ClassDeclaration
declarationForRecordType' isInner isSer aliases tparams elName parentName fields = do
    memberVars <- CM.mapM toMemberVar fields
    memberVars' <- CM.zipWithM addComment memberVars fields
    withMethods <- if L.length fields > 1
      then CM.mapM toWithMethod fields
      else pure []
    cons <- constructor
    tn <- if isInner then pure [] else do
      d <- constantDeclForTypeName aliases elName
      dfields <- CM.mapM (constantDeclForFieldType aliases) fields
      return (d:dfields)
    let comparableMethods = case parentName of
          Just pn -> if isSer then [variantCompareToMethod aliases tparams pn elName fields] else []
          Nothing -> if not isInner && isSer then [compareToMethod] else []
    let bodyDecls = tn ++ memberVars' ++ (noComment <$> [cons, equalsMethod, hashCodeMethod] ++ comparableMethods ++ withMethods)
    let ifaces = if isInner then serializableTypes isSer else interfaceTypes isSer aliases tparams elName
    return $ javaClassDeclaration aliases tparams elName classModsPublic Nothing ifaces bodyDecls
  where
    constructor = do
      params <- CM.mapM (fieldTypeToFormalParam aliases) fields
      let assignStmts = fieldToAssignStatement <$> fields
      return $ makeConstructor aliases elName False params assignStmts

    fieldToAssignStatement = Java.BlockStatementStatement . toAssignStmt . fieldTypeName

    fieldArgs = fieldNameToJavaExpression . fieldTypeName <$> fields

    toMemberVar (FieldType fname ft) = do
      let mods = [Java.FieldModifierPublic, Java.FieldModifierFinal]
      jt <- encodeType aliases S.empty ft
      let var = fieldNameToJavaVariableDeclarator fname
      return $ javaMemberField mods jt var

    toWithMethod field = do
        param <- fieldTypeToFormalParam aliases field
        return $ methodDeclaration mods [] anns methodName [param] result (Just [returnStmt])
      where
        anns = [] -- TODO
        mods = [Java.MethodModifierPublic]
        methodName = "with" ++ nonAlnumToUnderscores (capitalize (unName $ fieldTypeName field))
        result = referenceTypeToResult $ nameToJavaReferenceType aliases False [] elName Nothing
        consId = Java.Identifier $ sanitizeJavaName $ localNameOf elName
        returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just $
          javaConstructorCall (javaConstructorName consId Nothing) fieldArgs Nothing

    equalsMethod = methodDeclaration mods [] anns equalsMethodName [param] result $
        Just [instanceOfStmt,
          castStmt,
          returnAllFieldsEqual]
      where
        anns = [overrideAnnotation]
        mods = [Java.MethodModifierPublic]
        param = javaTypeToJavaFormalParameter (javaRefType [] Nothing "Object") (Name otherInstanceName)
        result = javaTypeToJavaResult javaBooleanType
        tmpName = "o"

        instanceOfStmt = Java.BlockStatementStatement $ Java.StatementIfThen $
            Java.IfThenStatement cond returnFalse
          where
            cond = javaUnaryExpressionToJavaExpression $
                Java.UnaryExpressionOther $
                Java.UnaryExpressionNotPlusMinusNot $
                javaRelationalExpressionToJavaUnaryExpression $
                javaInstanceOf other parent
              where
                other = javaIdentifierToJavaRelationalExpression $ javaIdentifier otherInstanceName
                parent = nameToJavaReferenceType aliases False [] elName Nothing

            returnFalse = javaReturnStatement $ Just $ javaBooleanExpression False

        castStmt = variableDeclarationStatement aliases jtype id rhs
          where
            jtype = javaTypeFromTypeName aliases elName
            id = javaIdentifier tmpName
            rhs = javaCastExpressionToJavaExpression $ javaCastExpression rt var
            var = javaIdentifierToJavaUnaryExpression $ Java.Identifier $ sanitizeJavaName otherInstanceName
            rt = nameToJavaReferenceType aliases False [] elName Nothing

        returnAllFieldsEqual = Java.BlockStatementStatement $ javaReturnStatement $ Just $ if L.null fields
            then javaBooleanExpression True
            else javaConditionalAndExpressionToJavaExpression $
              Java.ConditionalAndExpression (eqClause <$> fields)
          where
            eqClause (FieldType (Name fname) ftype)
              -- byte[]: use java.util.Arrays.equals() instead of equals()
              | isBinaryType ftype = arraysEqualsClause fname
              -- BigDecimal/BigInteger: use compareTo() == 0 instead of equals()
              | isBigNumericType ftype = compareToZeroClause fname
              | otherwise = equalsClause fname
            -- Use java.util.Objects.equals() for null-safe comparison
            -- Use this.fname to avoid name conflicts with the 'other' parameter in equals(Object other)
            equalsClause fname = javaPostfixExpressionToJavaInclusiveOrExpression $
                javaMethodInvocationToJavaPostfixExpression $ Java.MethodInvocation header [thisArg, otherArg]
              where
                thisArg = javaExpressionNameToJavaExpression $
                  fieldExpression (Java.Identifier "this") (javaIdentifier fname)
                otherArg = javaExpressionNameToJavaExpression $
                  fieldExpression (javaIdentifier tmpName) (javaIdentifier fname)
                header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex var [] (Java.Identifier equalsMethodName)
                var = Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "java.util.Objects"
            -- Generate: java.util.Arrays.equals(this.value, o.value)
            arraysEqualsClause fname = javaPostfixExpressionToJavaInclusiveOrExpression $
                javaMethodInvocationToJavaPostfixExpression $
                Java.MethodInvocation
                  (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                    (Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "java.util.Arrays")
                    [] (Java.Identifier equalsMethodName))
                  [ javaExpressionNameToJavaExpression $
                      fieldExpression (Java.Identifier "this") (javaIdentifier fname)
                  , javaExpressionNameToJavaExpression $
                      fieldExpression (javaIdentifier tmpName) (javaIdentifier fname)
                  ]
            -- Generate: this.field.compareTo(o.field) == 0
            compareToZeroClause fname = javaEqualityExpressionToJavaInclusiveOrExpression $
                Java.EqualityExpressionEqual $ Java.EqualityExpression_Binary lhs rhs
              where
                lhs = javaRelationalExpressionToJavaEqualityExpression $
                  javaPostfixExpressionToJavaRelationalExpression $
                  javaMethodInvocationToJavaPostfixExpression $
                  Java.MethodInvocation compareToHeader [compareToArg]
                rhs = javaPostfixExpressionToJavaRelationalExpression $
                  Java.PostfixExpressionPrimary $ javaLiteralToJavaPrimary $ javaInt 0
                compareToArg = javaExpressionNameToJavaExpression $
                  fieldExpression (javaIdentifier tmpName) (javaIdentifier fname)
                compareToHeader = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex compareToVar [] (Java.Identifier compareToMethodName)
                -- Use this.fname to avoid name conflicts with the 'other' parameter
                compareToVar = Java.MethodInvocation_VariantExpression $
                  fieldExpression (Java.Identifier "this") (javaIdentifier fname)

    hashCodeMethod = methodDeclaration mods [] anns hashCodeMethodName [] result $ Just [returnSum]
      where
        anns = [overrideAnnotation]
        mods = [Java.MethodModifierPublic]
        result = javaTypeToJavaResult javaIntType

        returnSum = Java.BlockStatementStatement $ if L.null fields
          then returnZero
          else javaReturnStatement $ Just $
            javaAdditiveExpressionToJavaExpression $ addExpressions $
              L.zipWith multPair multipliers (fieldTypeName <$> fields)
          where
            returnZero = javaReturnStatement $ Just $ javaIntExpression 0

            -- Use java.util.Objects.hashCode() for null-safe hashing
            multPair :: Int -> Name -> Java.MultiplicativeExpression
            multPair i (Name fname) = Java.MultiplicativeExpressionTimes $
                Java.MultiplicativeExpression_Binary lhs rhs
              where
                lhs = Java.MultiplicativeExpressionUnary $ javaPrimaryToJavaUnaryExpression $
                  javaLiteralToJavaPrimary $ javaInt i
                rhs = javaPostfixExpressionToJavaUnaryExpression $
                  javaMethodInvocationToJavaPostfixExpression $
                  Java.MethodInvocation
                    (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                      (Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "java.util.Objects")
                      [] (Java.Identifier hashCodeMethodName))
                    [javaExpressionNameToJavaExpression $ Java.ExpressionName Nothing $ Java.Identifier $ sanitizeJavaName fname]

            multipliers = L.cycle first20Primes
              where
                first20Primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71]

    compareToMethod = recordCompareToMethod aliases tparams elName fields

-- | Check whether a Hydra type maps to a Java type that does not implement Comparable.
-- These types need special comparison logic in compareTo methods.
isNonComparableType :: Type -> Bool
isNonComparableType typ = case deannotateType typ of
  TypeList _ -> True
  TypeSet _ -> True
  TypeMap _ -> True
  TypeMaybe _ -> True
  TypePair _ -> True
  TypeEither _ -> True
  TypeFunction _ -> True
  TypeLiteral LiteralTypeBinary -> True  -- byte[] is not Comparable
  TypeForall (ForallType _ body) -> isNonComparableType body
  _ -> False

-- | Check whether a Hydra type is the binary literal type (maps to byte[]).
isBinaryType :: Type -> Bool
isBinaryType typ = case deannotateType typ of
  TypeLiteral LiteralTypeBinary -> True
  _ -> False

-- | Check whether a Hydra type maps to BigDecimal or BigInteger in Java.
--   These types require compareTo() instead of equals() for value comparison,
--   because BigDecimal.equals() considers scale (e.g. 0.0 != 0).
isBigNumericType :: Type -> Bool
isBigNumericType typ = case deannotateType typ of
  TypeLiteral (LiteralTypeFloat FloatTypeBigfloat) -> True
  TypeLiteral (LiteralTypeInteger IntegerTypeBigint) -> True
  _ -> False

-- | Generate a compareTo method for a record type.
-- For zero fields: return 0
-- For one field: return ((Comparable) this.field).compareTo(other.field)
-- For multiple fields: sequential comparison with early return on non-zero
-- For non-Comparable fields (List, Set, Map, etc.): use Integer.compare(hashCode, hashCode)
-- For byte[] fields: use java.util.Arrays.compare
recordCompareToMethod :: Aliases -> [Java.TypeParameter] -> Name -> [FieldType] -> Java.ClassBodyDeclaration
recordCompareToMethod aliases tparams elName fields = methodDeclaration mods [] anns compareToMethodName [param] result $
    Just body
  where
    anns = [overrideAnnotation, suppressWarningsUncheckedAnnotation]
    mods = [Java.MethodModifierPublic]
    param = javaTypeToJavaFormalParameter (javaTypeFromTypeName aliases elName) (Name otherInstanceName)
    result = javaTypeToJavaResult javaIntType
    body = case fields of
      [] -> [Java.BlockStatementStatement $ javaReturnStatement $ Just $ javaIntExpression 0]
      [f] -> [Java.BlockStatementStatement $ javaReturnStatement $ Just $ compareField f]
      _ -> cmpDecl : L.concatMap compareAndReturn (L.init fields) ++ [returnLast (L.last fields)]
    cmpVarName = "cmp"
    -- int cmp;
    cmpDecl = variableDeclarationStatement aliases javaIntType (javaIdentifier cmpVarName) (javaIntExpression 0)
    -- Generate the appropriate comparison expression for a field based on its type
    compareField (FieldType (Name fname) ftype)
      -- byte[] fields: java.util.Arrays.compare(this.field, other.field)
      | isBinaryType ftype = arraysCompareExpr fname
      -- Non-Comparable fields: Integer.compare(this.field.hashCode(), other.field.hashCode())
      | isNonComparableType ftype = hashCodeCompareExpr fname
      -- Comparable fields: ((Comparable) this.field).compareTo(other.field)
      | otherwise = comparableCompareExpr fname
    -- ((Comparable) this.field).compareTo(other.field)
    comparableCompareExpr fname =
        javaMethodInvocationToJavaExpression $ Java.MethodInvocation header [arg]
      where
        arg = javaExpressionNameToJavaExpression $
          fieldExpression (javaIdentifier otherInstanceName) (javaIdentifier fname)
        header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex castVar [] (Java.Identifier compareToMethodName)
        castVar = Java.MethodInvocation_VariantPrimary $ javaExpressionToJavaPrimary $
          javaCastExpressionToJavaExpression $
          javaCastExpression comparableRefType (javaIdentifierToJavaUnaryExpression $ Java.Identifier $ sanitizeJavaName fname)
    -- java.util.Arrays.compare(this.field, other.field)
    arraysCompareExpr fname =
        javaMethodInvocationToJavaExpression $ Java.MethodInvocation
          (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
            (Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "java.util.Arrays")
            [] (Java.Identifier "compare"))
          [ javaExpressionNameToJavaExpression $ Java.ExpressionName Nothing $ Java.Identifier $ sanitizeJavaName fname
          , javaExpressionNameToJavaExpression $
              fieldExpression (javaIdentifier otherInstanceName) (javaIdentifier fname)
          ]
    -- Integer.compare(this.field.hashCode(), other.field.hashCode())
    hashCodeCompareExpr fname =
        javaMethodInvocationToJavaExpression $ Java.MethodInvocation
          (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
            (Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "Integer")
            [] (Java.Identifier "compare"))
          [ javaMethodInvocationToJavaExpression $ Java.MethodInvocation
              (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                (Java.MethodInvocation_VariantExpression $
                  Java.ExpressionName Nothing $ Java.Identifier $ sanitizeJavaName fname)
                [] (Java.Identifier hashCodeMethodName))
              []
          , javaMethodInvocationToJavaExpression $ Java.MethodInvocation
              (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                (Java.MethodInvocation_VariantExpression $
                  fieldExpression (javaIdentifier otherInstanceName) (javaIdentifier fname))
                [] (Java.Identifier hashCodeMethodName))
              []
          ]
    comparableRefType = Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
      Java.ClassType [] Java.ClassTypeQualifierNone (javaTypeIdentifier "Comparable") []
    -- cmp = compareField(f); if (cmp != 0) return cmp;
    compareAndReturn f =
      [ Java.BlockStatementStatement $ javaAssignmentStatement
          (Java.LeftHandSideExpressionName $ Java.ExpressionName Nothing $ javaIdentifier cmpVarName)
          (compareField f)
      , Java.BlockStatementStatement $ Java.StatementIfThen $ Java.IfThenStatement
          (cmpNotZero)
          (javaReturnStatement $ Just $ javaExpressionNameToJavaExpression $ Java.ExpressionName Nothing $ javaIdentifier cmpVarName)
      ]
    returnLast f = Java.BlockStatementStatement $ javaReturnStatement $ Just $ compareField f
    -- cmp != 0
    cmpNotZero = javaEqualityExpressionToJavaExpression $
        Java.EqualityExpressionNotEqual $ Java.EqualityExpression_Binary lhs rhs
      where
        lhs = javaRelationalExpressionToJavaEqualityExpression $
          javaPostfixExpressionToJavaRelationalExpression $
          Java.PostfixExpressionName $ Java.ExpressionName Nothing $ javaIdentifier cmpVarName
        rhs = javaPostfixExpressionToJavaRelationalExpression $
          Java.PostfixExpressionPrimary $ javaLiteralToJavaPrimary $ javaInt 0

-- | Generate a compareTo method for a union variant (inner) class.
-- Takes the parent type as the compareTo parameter.
-- First compares variant class names for tag ordering,
-- then casts 'other' to the same variant class and compares the 'value' field.
variantCompareToMethod :: Aliases -> [Java.TypeParameter] -> Name -> Name -> [FieldType] -> Java.ClassBodyDeclaration
variantCompareToMethod aliases tparams parentName variantName fields = methodDeclaration mods [] anns compareToMethodName [param] result $
    Just body
  where
    anns = [overrideAnnotation, suppressWarningsUncheckedAnnotation]
    mods = [Java.MethodModifierPublic]
    -- Parameter type is the parent type, not the variant type
    param = javaTypeToJavaFormalParameter (javaTypeFromTypeName aliases parentName) (Name otherInstanceName)
    result = javaTypeToJavaResult javaIntType
    body =
      [ -- int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
        variableDeclarationStatement aliases javaIntType (javaIdentifier "tagCmp") tagCompareExpr
      , -- if (tagCmp != 0) return tagCmp;
        Java.BlockStatementStatement $ Java.StatementIfThen $ Java.IfThenStatement
          tagCmpNotZero
          (javaReturnStatement $ Just $ javaExpressionNameToJavaExpression $ Java.ExpressionName Nothing $ javaIdentifier "tagCmp")
      ] ++ valueCompareStmt
    -- For unit variants (no fields): return 0
    -- For non-unit variants: VariantName o = (VariantName) other; return ((Comparable) this.value).compareTo(o.value);
    valueCompareStmt = case fields of
      [] -> [Java.BlockStatementStatement $ javaReturnStatement $ Just $ javaIntExpression 0]
      _ ->
        [ -- VariantName o = (VariantName) other;
          variableDeclarationStatement aliases variantJavaType (javaIdentifier varTmpName) castOtherExpr
        ] ++ case fields of
          [f] -> [Java.BlockStatementStatement $ javaReturnStatement $ Just $ compareFieldToOther f]
          _ -> cmpDecl : L.concatMap compareAndReturn (L.init fields) ++ [returnLast (L.last fields)]
    varTmpName = "o"
    variantJavaType = javaTypeFromTypeName aliases variantName
    variantRefType = nameToJavaReferenceType aliases False [] variantName Nothing
    castOtherExpr = javaCastExpressionToJavaExpression $
      javaCastExpression variantRefType (javaIdentifierToJavaUnaryExpression $ Java.Identifier otherInstanceName)
    -- Generate type-aware comparison expression for a field
    compareFieldToOther (FieldType (Name fname) ftype)
      | isBinaryType ftype = arraysCompareExpr fname
      | isNonComparableType ftype = hashCodeCompareExpr fname
      | otherwise = comparableCompareExpr fname
    -- ((Comparable) this.field).compareTo(o.field)
    comparableCompareExpr fname =
        javaMethodInvocationToJavaExpression $ Java.MethodInvocation header [arg]
      where
        arg = javaExpressionNameToJavaExpression $
          fieldExpression (javaIdentifier varTmpName) (javaIdentifier fname)
        header = Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex castVar [] (Java.Identifier compareToMethodName)
        castVar = Java.MethodInvocation_VariantPrimary $ javaExpressionToJavaPrimary $
          javaCastExpressionToJavaExpression $
          javaCastExpression comparableRefType (javaIdentifierToJavaUnaryExpression $ Java.Identifier $ sanitizeJavaName fname)
    -- java.util.Arrays.compare(this.field, o.field)
    arraysCompareExpr fname =
        javaMethodInvocationToJavaExpression $ Java.MethodInvocation
          (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
            (Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "java.util.Arrays")
            [] (Java.Identifier "compare"))
          [ javaExpressionNameToJavaExpression $ Java.ExpressionName Nothing $ Java.Identifier $ sanitizeJavaName fname
          , javaExpressionNameToJavaExpression $
              fieldExpression (javaIdentifier varTmpName) (javaIdentifier fname)
          ]
    -- Integer.compare(this.field.hashCode(), o.field.hashCode())
    hashCodeCompareExpr fname =
        javaMethodInvocationToJavaExpression $ Java.MethodInvocation
          (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
            (Java.MethodInvocation_VariantType $ javaTypeName $ Java.Identifier "Integer")
            [] (Java.Identifier "compare"))
          [ javaMethodInvocationToJavaExpression $ Java.MethodInvocation
              (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                (Java.MethodInvocation_VariantExpression $
                  Java.ExpressionName Nothing $ Java.Identifier $ sanitizeJavaName fname)
                [] (Java.Identifier hashCodeMethodName))
              []
          , javaMethodInvocationToJavaExpression $ Java.MethodInvocation
              (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                (Java.MethodInvocation_VariantExpression $
                  fieldExpression (javaIdentifier varTmpName) (javaIdentifier fname))
                [] (Java.Identifier hashCodeMethodName))
              []
          ]
    comparableRefType = Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
      Java.ClassType [] Java.ClassTypeQualifierNone (javaTypeIdentifier "Comparable") []
    cmpVarName = "cmp"
    cmpDecl = variableDeclarationStatement aliases javaIntType (javaIdentifier cmpVarName) (javaIntExpression 0)
    compareAndReturn f =
      [ Java.BlockStatementStatement $ javaAssignmentStatement
          (Java.LeftHandSideExpressionName $ Java.ExpressionName Nothing $ javaIdentifier cmpVarName)
          (compareFieldToOther f)
      , Java.BlockStatementStatement $ Java.StatementIfThen $ Java.IfThenStatement
          (cmpNotZero)
          (javaReturnStatement $ Just $ javaExpressionNameToJavaExpression $ Java.ExpressionName Nothing $ javaIdentifier cmpVarName)
      ]
    returnLast f = Java.BlockStatementStatement $ javaReturnStatement $ Just $ compareFieldToOther f
    cmpNotZero = javaEqualityExpressionToJavaExpression $
        Java.EqualityExpressionNotEqual $ Java.EqualityExpression_Binary lhs rhs
      where
        lhs = javaRelationalExpressionToJavaEqualityExpression $
          javaPostfixExpressionToJavaRelationalExpression $
          Java.PostfixExpressionName $ Java.ExpressionName Nothing $ javaIdentifier cmpVarName
        rhs = javaPostfixExpressionToJavaRelationalExpression $
          Java.PostfixExpressionPrimary $ javaLiteralToJavaPrimary $ javaInt 0
    thisPrimary = Java.PrimaryNoNewArray_ Java.PrimaryNoNewArrayThis
    tagCompareExpr = javaMethodInvocationToJavaExpression $
      Java.MethodInvocation
        (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
          (Java.MethodInvocation_VariantPrimary $ javaMethodInvocationToJavaPrimary $
            Java.MethodInvocation
              (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                (Java.MethodInvocation_VariantPrimary $ javaMethodInvocationToJavaPrimary $
                  Java.MethodInvocation
                    (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                      (Java.MethodInvocation_VariantPrimary thisPrimary)
                      [] (Java.Identifier "getClass"))
                    [])
                [] (Java.Identifier "getName"))
              [])
          [] (Java.Identifier compareToMethodName))
        [ javaMethodInvocationToJavaExpression $
            Java.MethodInvocation
              (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                (Java.MethodInvocation_VariantPrimary $ javaMethodInvocationToJavaPrimary $
                  Java.MethodInvocation
                    (Java.MethodInvocation_HeaderComplex $ Java.MethodInvocation_Complex
                      (Java.MethodInvocation_VariantExpression $
                        Java.ExpressionName Nothing $ Java.Identifier otherInstanceName)
                      [] (Java.Identifier "getClass"))
                    [])
                [] (Java.Identifier "getName"))
              []
        ]
    tagCmpNotZero = javaEqualityExpressionToJavaExpression $
        Java.EqualityExpressionNotEqual $ Java.EqualityExpression_Binary lhs rhs
      where
        lhs = javaRelationalExpressionToJavaEqualityExpression $
          javaPostfixExpressionToJavaRelationalExpression $
          Java.PostfixExpressionName $ Java.ExpressionName Nothing $ javaIdentifier "tagCmp"
        rhs = javaPostfixExpressionToJavaRelationalExpression $
          Java.PostfixExpressionPrimary $ javaLiteralToJavaPrimary $ javaInt 0

declarationForType :: Bool -> Aliases -> (Binding, TypeApplicationTerm) -> Flow Graph Java.TypeDeclarationWithComments
declarationForType isSer aliases (el, TypeApplicationTerm term _) = withTrace ("element " ++ unName (bindingName el)) $ do
    g <- Monads.getState
    t <- Monads.eitherToFlow Util.unDecodingError (DecodeCore.type_ g term) >>= adaptTypeToLanguage javaLanguage
    cd <- toClassDecl False isSer aliases [] (bindingName el) t
    comments <- commentsFromElement el
    return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass cd) comments

declarationForUnionType :: Bool -> Aliases
  -> [Java.TypeParameter] -> Name -> [FieldType] -> Flow Graph Java.ClassDeclaration
declarationForUnionType isSer aliases tparams elName fields = do
    variantClasses <- CM.mapM (fmap augmentVariantClass . unionFieldClass) fields
    let variantDecls = Java.ClassBodyDeclarationClassMember . Java.ClassMemberDeclarationClass <$> variantClasses
    variantDecls' <- CM.zipWithM addComment variantDecls fields
    let otherDecls = noComment <$> [privateConstructor, toAcceptMethod True tparams, visitor, partialVisitor]
    tn <- do
      d <- constantDeclForTypeName aliases elName
      dfields <- CM.mapM (constantDeclForFieldType aliases) fields
      return (d:dfields)
    -- No compareTo on the abstract parent — each variant overrides compareTo with proper value comparison
    let bodyDecls = tn ++ otherDecls ++ variantDecls'
    let mods = classModsPublic ++ [Java.ClassModifierAbstract]
    return $ javaClassDeclaration aliases tparams elName mods Nothing (interfaceTypes isSer aliases tparams elName) bodyDecls
  where
    privateConstructor = makeConstructor aliases elName True [] []
    unionFieldClass (FieldType fname ftype) = do
      let rfields = if Schemas.isUnitType (deannotateType ftype) then [] else [FieldType (Name valueFieldName) $ deannotateType ftype]
      let varName = variantClassName False elName fname
      declarationForRecordType' True isSer aliases [] varName (if isSer then Just elName else Nothing) rfields
    augmentVariantClass (Java.ClassDeclarationNormal cd) = Java.ClassDeclarationNormal $ cd {
        Java.normalClassDeclarationModifiers = [Java.ClassModifierPublic, Java.ClassModifierStatic, Java.ClassModifierFinal],
        Java.normalClassDeclarationExtends = Just $ nameToJavaClassType aliases True args elName Nothing,
        Java.normalClassDeclarationParameters = tparams,
        Java.normalClassDeclarationBody = newBody (Java.normalClassDeclarationBody cd)}
      where
        newBody (Java.ClassBody decls) = Java.ClassBody $ decls ++ [noComment $ toAcceptMethod False tparams]
        args = typeParameterToTypeArgument <$> tparams

    visitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration mods ti vtparams extends body
      where
        mods = [Java.InterfaceModifierPublic]
        ti = Java.TypeIdentifier $ Java.Identifier visitorName
        vtparams = tparams ++ [javaTypeParameter visitorReturnParameter]
        extends = []
        body = Java.InterfaceBody (toVisitMethod . fieldTypeName <$> fields)
          where
            toVisitMethod fname = interfaceMethodDeclaration [] [] visitMethodName [variantInstanceParam fname] resultR Nothing

    partialVisitor = javaInterfaceDeclarationToJavaClassBodyDeclaration $
        Java.NormalInterfaceDeclaration {
            Java.normalInterfaceDeclarationModifiers = [Java.InterfaceModifierPublic],
            Java.normalInterfaceDeclarationIdentifier = Java.TypeIdentifier $ Java.Identifier partialVisitorName,
            Java.normalInterfaceDeclarationParameters = tparams ++ [javaTypeParameter visitorReturnParameter],
            Java.normalInterfaceDeclarationExtends =
              [Java.InterfaceType $ javaClassType ((typeParameterToReferenceType <$> tparams) ++ [visitorTypeVariable]) Nothing visitorName],
            Java.normalInterfaceDeclarationBody = Java.InterfaceBody $ otherwise:(toVisitMethod . fieldTypeName <$> fields)}
      where
        otherwise = interfaceMethodDeclaration defaultMod [] otherwiseMethodName [mainInstanceParam] resultR $ Just [throw]
          where
            typeArgs = typeParameterToTypeArgument <$> tparams
            throw = Java.BlockStatementStatement $ javaThrowIllegalStateException args
              where
                args = [javaAdditiveExpressionToJavaExpression $ addExpressions [
                  javaStringMultiplicativeExpression "Non-exhaustive patterns when matching: ",
                  Java.MultiplicativeExpressionUnary $ javaIdentifierToJavaUnaryExpression $ Java.Identifier "instance"]]

        toVisitMethod fname = interfaceMethodDeclaration defaultMod [] visitMethodName [variantInstanceParam fname] resultR $
            Just [returnOtherwise]
          where
            returnOtherwise = Java.BlockStatementStatement $ javaReturnStatement $ Just $
              javaPrimaryToJavaExpression $ Java.PrimaryNoNewArray_ $ Java.PrimaryNoNewArrayMethodInvocation $
              methodInvocation Nothing (Java.Identifier otherwiseMethodName) [javaIdentifierToJavaExpression $ Java.Identifier "instance"]

    defaultMod = [Java.InterfaceMethodModifierDefault]

    resultR = javaTypeToJavaResult $ Java.TypeReference visitorTypeVariable

    typeArgs = typeParameterToTypeArgument <$> tparams

    mainInstanceParam = javaTypeToJavaFormalParameter classRef $ Name instanceName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False typeArgs elName Nothing

    variantInstanceParam fname = javaTypeToJavaFormalParameter classRef $ Name instanceName
      where
        classRef = javaClassTypeToJavaType $
          nameToJavaClassType aliases False typeArgs (variantClassName False elName fname) Nothing

elementJavaIdentifier :: Bool -> Bool -> Aliases -> Name -> Java.Identifier
elementJavaIdentifier isPrim isMethod aliases name = Java.Identifier $ if isPrim
    then (qualify $ capitalize local) ++ "." ++ applyMethodName
    else case ns of
      Nothing -> sanitizeJavaName local
      Just n -> (qualify $ elementsClassName n) ++ sep ++ sanitizeJavaName local
  where
    sep = if isMethod then "::" else "."
    qualify s = Java.unIdentifier $ nameToJavaName aliases $ unqualifyName $ QualifiedName ns s
    QualifiedName ns local = qualifyName name

elementsClassName :: Namespace -> String
elementsClassName (Namespace ns) = sanitizeJavaName $ capitalize $ L.last $ LS.splitOn "." ns

encodeApplication :: JavaEnvironment -> Application -> Flow Graph Java.Expression
encodeApplication env app@(Application lhs rhs) = do
    -- Get the function's arity from its type
    -- Try to get type from annotations first (works better with hoisted terms)
    mfunTyp <- getType (termAnnotationInternal fun)
    funTyp <- case mfunTyp of
      Just t -> pure t
      Nothing -> tryTypeOf "1" tc fun
    let arity = typeArity funTyp
    -- Annotate lambda arguments with expected types from the callee's type scheme.
    -- This fixes incorrect type annotations created by normalizeTypeVariablesInTerm.
    let calleeName = case deannotateTerm fun of
          TermFunction (FunctionPrimitive n) -> Just n
          TermVariable n -> Just n
          _ -> Nothing
    annotatedArgs <- case calleeName of
      Nothing -> return args
      Just cname -> annotateLambdaArgs cname typeApps args
    -- Generate the initial call and apply remaining arguments
    -- gatherArgs already stripped type applications, just remove any remaining annotations
    case deannotateTerm fun of
      TermFunction f -> case f of
        FunctionPrimitive name -> do
          let hargs = L.take arity annotatedArgs
              rargs = L.drop arity annotatedArgs
          initialCall <- functionCall env True name hargs []
          applyRemaining initialCall rargs
        _ -> fallback
      TermVariable name ->
        -- Check if this is a recursive let-bound variable (and not shadowed by a lambda parameter)
        -- Recursive variables need curried construction with .get()
        if isRecursiveVariable aliases name && not (isLambdaBoundIn name (aliasesLambdaVars aliases))
          then fallback  -- Use curried construction for recursive bindings
          else do
            -- For hoisted methods, the method's parameter count may be less than typeArity
            -- because the method returns a function. Use the method arity for the split.
            symClass <- classifyDataReference name
            let methodArity = case symClass of
                  JavaSymbolClassHoistedLambda n -> n
                  _ -> arity
            let hargs = L.take methodArity annotatedArgs
                rargs = L.drop methodArity annotatedArgs
            -- Filter type applications: drop all type args if any references
            -- a type variable not in scope (e.g. phantom vars from over-generalized schemes).
            -- Also check that in-scope vars are trusted.
            let trusted = aliasesTrustedTypeVars aliases
                inScope = aliasesInScopeTypeParams aliases
                filteredTypeApps = if S.null trusted || S.null inScope then []
                  else let allVars = S.unions $ fmap collectTypeVars typeApps
                       in if not (S.isSubsetOf allVars inScope) then []
                          else if S.isSubsetOf allVars trusted then typeApps else []
            -- Correct the type application ordering by unifying with the callee's type scheme.
            -- The IR's type applications can be in the wrong order due to normalizeTypeVariablesInTerm.
            safeTypeApps <- if L.null filteredTypeApps then return []
              else do
                result <- correctTypeApps tc name hargs filteredTypeApps
                return result
            -- Filter phantom type args: remove type args at positions corresponding to
            -- scheme variables that don't appear in the callee's actual type.
            finalTypeApps <- filterPhantomTypeArgs name safeTypeApps
            initialCall <- functionCall env False name hargs finalTypeApps
            applyRemaining initialCall rargs
      _ -> fallback
  where
    aliases = javaEnvironmentAliases env
    tc = javaEnvironmentTypeContext env
    encode = encodeTerm env
    (fun, (args, typeApps)) = gatherArgsWithTypeApps (TermApplication app) [] []

    -- | Annotate lambda arguments with expected types computed from the callee's type scheme
    -- and type applications. This corrects type annotations that normalizeTypeVariablesInTerm
    -- may have made inconsistent with the outer scope.
    annotateLambdaArgs :: Name -> [Type] -> [Term] -> Flow Graph [Term]
    annotateLambdaArgs cname tApps argTerms = if L.null tApps then return argTerms
      else do
        -- Look up the type scheme from either elements or primitives
        mts <- do
          mel <- dereferenceElement cname
          case mel of
            Just el -> return $ bindingType el
            Nothing -> do
              g <- Monads.getState
              return $ fmap primitiveType $ M.lookup cname (graphPrimitives g)
        case mts of
          Nothing -> return argTerms
          Just ts -> do
            let schemeType = typeSchemeType ts
                -- Filter scheme vars to only those used in the scheme type,
                -- matching the filtering done in encodeTermDefinition.
                schemeTypeVars = collectTypeVars schemeType
                schemeVars = L.filter (\v -> S.member v schemeTypeVars) $ typeSchemeVariables ts
            if L.null schemeVars || L.length schemeVars /= L.length tApps
              then return argTerms
              else do
                let subst = M.fromList $ L.zip schemeVars tApps
                    expectedTypes = peelExpectedTypes subst (L.length argTerms) schemeType
                return $ L.zipWith (\arg mExpected ->
                      propagateType mExpected arg
                  ) argTerms (expectedTypes ++ repeat (TypeVariable $ Name "unused"))

    -- Apply remaining arguments one at a time using .apply()
    applyRemaining exp remArgs = case remArgs of
      [] -> pure exp
      (h:r) -> do
        jarg <- encode h
        applyRemaining (apply exp jarg) r

    fallback = withTrace "fallback" $ do
        -- Try to get type from annotations first (works better with hoisted terms)
        mt <- getType (termAnnotationInternal lhs)
        t <- case mt of
          Just typ -> pure typ
          Nothing -> tryTypeOf "2" tc lhs
        -- Get domain and codomain
        (dom, cod) <- case deannotateTypeParameters $ deannotateType t of
          TypeFunction (FunctionType dom cod) -> pure (dom, cod)
          t2 -> fail $ "Unexpected type: " ++ ShowCore.type_ t2
        case deannotateTerm lhs of
          TermFunction f -> case f of
            FunctionElimination e -> do
                jarg <- encode rhs
                -- If dom has no type args, try to get a richer type from the argument (rhs).
                -- This handles cases where the elimination type annotation loses type parameters
                -- (e.g., ParseResult instead of ParseResult<T0>).
                enrichedDom <- if not (L.null $ javaTypeArgumentsForType dom) then return dom
                  else do
                    mrt <- getType (termAnnotationInternal rhs)
                    case mrt of
                      Just rt | not (L.null $ javaTypeArgumentsForType rt) -> return rt
                      _ -> do
                        rt <- tryTypeOf "dom-enrich" tc rhs
                        if not (L.null $ javaTypeArgumentsForType rt) then return rt
                        else return dom
                encodeElimination env (Just jarg) enrichedDom cod e
            _ -> defaultExpression
          _ -> defaultExpression
      where
        defaultExpression = do
          -- Note: the domain type will not be used, so we just substitute the unit type
          jfun <- encode lhs
          jarg <- encode rhs
          let prim = javaExpressionToJavaPrimary jfun
          return $ apply jfun jarg
    apply exp jarg = javaMethodInvocationToJavaExpression $
      methodInvocation (Just $ Right $ javaExpressionToJavaPrimary exp) (Java.Identifier applyMethodName) [jarg]

-- | Convert definitions directly to Java compilation units (simple adapter approach)
encodeDefinitions :: Module -> [Definition] -> Flow Graph (M.Map Name Java.CompilationUnit)
encodeDefinitions mod defs = do
    g <- getState
    tc <- initialTypeContext g
    let env = JavaEnvironment {
                javaEnvironmentAliases = aliases,
                javaEnvironmentTypeContext = tc}
    let pkg = javaPackageDeclaration $ moduleNamespace mod
    let nonTypedefDefs = L.filter (not . isTypedef) typeDefs
    typeUnits <- CM.mapM (encodeTypeDefinition pkg aliases) nonTypedefDefs
    termUnits <- if L.null termDefs
      then return []
      else do
        dataMembers <- CM.mapM (encodeTermDefinition env) termDefs
        return [constructElementsInterface mod dataMembers]
    let units = typeUnits ++ termUnits
    return $ M.fromList units
  where
    (typeDefs, termDefs) = partitionDefinitions defs
    aliases = importAliasesForModule mod
    -- A typedef is a type definition that is not a record, union, or wrap type.
    -- For example: Vertex = int32. These are treated as transparent aliases in Java.
    isTypedef (TypeDefinition _ typ) = case stripForallsAndAnnotations typ of
      TypeRecord _ -> False
      TypeUnion _ -> False
      TypeWrap _ -> False
      _ -> True
    stripForallsAndAnnotations t = case deannotateType t of
      TypeForall (ForallType _ body) -> stripForallsAndAnnotations body
      other -> other

encodeElimination :: JavaEnvironment -> Maybe Java.Expression -> Type -> Type -> Elimination -> Flow Graph Java.Expression
encodeElimination env marg dom cod elm = case elm of
    EliminationRecord (Projection _ fname) -> do
      jdomr <- encodeType aliases S.empty dom >>= javaTypeToJavaReferenceType
      jexp <- case marg of
        Nothing -> pure $ javaLambda var jbody
          where
            var = Name "projected"
            jbody = javaExpressionNameToJavaExpression $
              fieldExpression (variableToJavaIdentifier var) (javaIdentifier $ unName fname)
        Just jarg -> pure $ javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier $ unName fname)
          where
            qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
      return jexp
    EliminationUnion (CaseStatement tname def fields) -> do
       case marg of
        Nothing -> do
          g <- getState
          let lhs = TermFunction $ FunctionElimination elm
          let var = Name "u"
          let typedLambda = TermFunction $ FunctionLambda $ Lambda var (Just dom) $
                TermApplication $ Application lhs (TermVariable var)
          encodeTerm env typedLambda
          -- TODO: default value
        Just jarg -> applyElimination jarg
      where
        applyElimination jarg = do
            let prim = javaExpressionToJavaPrimary jarg
            let consId = innerClassRef aliases tname partialVisitorName
            jcod <- encodeType aliases S.empty cod
            rt <- javaTypeToJavaReferenceType jcod
            domArgs <- domTypeArgs dom
            let targs = typeArgsOrDiamond $ domArgs ++ [Java.TypeArgumentReference rt]
            otherwiseBranches <- case def of
              Nothing -> pure []
              Just d -> do
                b <- otherwiseBranch jcod d
                return [b]
            visitBranches <- CM.mapM (visitBranch jcod) fields
            let body = Java.ClassBody $ otherwiseBranches ++ visitBranches
            let visitor = javaConstructorCall (javaConstructorName consId $ Just targs) [] (Just body)
            return $ javaMethodInvocationToJavaExpression $
              methodInvocation (Just $ Right prim) (Java.Identifier acceptMethodName) [visitor]
          where
            -- Extract Java type arguments from dom, using actual type application args
            -- (e.g., ParseResult<Function<T0,T1>> -> [Function<T0,T1>]) rather than free vars.
            domTypeArgs d = case extractTypeApplicationArgs (deannotateType d) of
              args@(_:_) -> CM.mapM (\t -> do
                jt <- encodeType aliases S.empty t
                rt <- javaTypeToJavaReferenceType jt
                return $ Java.TypeArgumentReference rt) args
              [] -> return $ javaTypeArgumentsForType d

            -- Annotate a term body with the expected codomain type, propagating through
            -- applications so that inner type-applied subterms also get correct annotations.
            -- This fixes cases where inference over-generalizes type variables in hoisted
            -- bindings, causing inner casts to use the wrong type variable.
            annotateBodyWithCod typ term = case term of
              -- For type applications, annotate the whole thing with the expected type
              TermTypeApplication _ -> setTermAnnotation key_type (Just $ EncodeCore.type_ typ) term
              -- For applications like Pure.apply(arg), annotate the application with the
              -- overall type, and also annotate arguments that have type applications
              TermApplication (Application lhs rhs) ->
                let annotatedRhs = case deannotateTerm rhs of
                      TermTypeApplication _ -> annotateBodyWithCod (extractArgType lhs typ) rhs
                      _ -> rhs
                in setTermAnnotation key_type (Just $ EncodeCore.type_ typ) $
                   TermApplication $ Application lhs annotatedRhs
              -- Default: just annotate the outermost term
              _ -> setTermAnnotation key_type (Just $ EncodeCore.type_ typ) term

            -- Try to extract the argument type from a function application.
            -- For a function like Pure :: a -> Flow s a with return type Flow s a,
            -- the argument type is a (the second type parameter of Flow).
            extractArgType _ (TypeApplication (ApplicationType (TypeApplication (ApplicationType _ _)) argType)) = argType
            extractArgType _ typ = typ

            otherwiseBranch jcod d = do
              targs <- domTypeArgs dom
              let jdom = Java.TypeReference $ nameToJavaReferenceType aliases True targs tname Nothing
              let mods = [Java.MethodModifierPublic]
              let anns = [overrideAnnotation]
              let param = javaTypeToJavaFormalParameter jdom $ Name instanceName
              let result = Java.ResultType $ Java.UnannType jcod
              -- Analyze the term for bindings (handles nested lets)
              fs <- analyzeJavaFunction env d
              let bindings = functionStructureBindings fs
                  rawBody = functionStructureBody fs
                  -- Annotate the body with the case codomain type to override type application
                  -- casts that may use a different type variable when inference over-generalizes.
                  innerBody = annotateBodyWithCod cod rawBody
                  env2 = functionStructureEnvironment fs
              -- Convert bindings to Java block statements
              (bindingStmts, env3) <- bindingsToStatements env2 bindings
              -- Encode the body (now without nested lets)
              jret <- encodeTerm env3 innerBody
              let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret
              let allStmts = bindingStmts ++ [returnStmt]
              return $ noComment $ methodDeclaration mods [] anns otherwiseMethodName [param] result (Just allStmts)

            visitBranch jcod field = do
              -- Extract type arguments from the dom type. Use actual type application args
              -- when available (to handle complex types like ParseResult<Function<T0,T1>>),
              -- falling back to javaTypeArgumentsForType for simple type variable args.
              targs <- case extractTypeApplicationArgs (deannotateType dom) of
                args@(_:_) -> do
                  jargs <- CM.mapM (\t -> do
                    jt <- encodeType aliases S.empty t
                    rt <- javaTypeToJavaReferenceType jt
                    return $ Java.TypeArgumentReference rt) args
                  return jargs
                [] -> return $ javaTypeArgumentsForType dom
              let jdom = Java.TypeReference $ nameToJavaReferenceType aliases True targs tname (Just $ capitalize $ unName $ fieldName field)
              let mods = [Java.MethodModifierPublic]
              let anns = [overrideAnnotation]
              let result = Java.ResultType $ Java.UnannType jcod

              -- Field terms are lambdas; apply to special var that encodes to instance.value
              case deannotateTerm (fieldTerm field) of
                TermFunction (FunctionLambda lam@(Lambda lambdaParam mdom body)) -> withLambda env lam $ \env2 -> do
                  let env3 = insertBranchVar lambdaParam env2

                  fs <- analyzeJavaFunction env3 body
                  let bindings = functionStructureBindings fs
                      innerBody = functionStructureBody fs
                      env4 = functionStructureEnvironment fs

                  (bindingStmts, env5) <- bindingsToStatements env4 bindings
                  jret <- encodeTerm env5 innerBody

                  let param = javaTypeToJavaFormalParameter jdom lambdaParam
                  let returnStmt = Java.BlockStatementStatement $ javaReturnStatement $ Just jret
                  let allStmts = bindingStmts ++ [returnStmt]
                  return $ noComment $ methodDeclaration mods [] anns visitMethodName [param] result (Just allStmts)
                _ -> fail $ "visitBranch: field term is not a lambda: " ++ ShowCore.term (fieldTerm field)
    EliminationWrap name -> pure $ case marg of
        Nothing -> javaLambda var jbody
          where
            var = Name "wrapped"
            arg = javaIdentifierToJavaExpression $ variableToJavaIdentifier var
            jbody = withArg arg
        Just jarg -> withArg jarg
      where
        withArg jarg = javaFieldAccessToJavaExpression $ Java.FieldAccess qual (javaIdentifier valueFieldName)
          where
            qual = Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary jarg
  where
    aliases = javaEnvironmentAliases env

encodeFunction :: JavaEnvironment -> Type -> Type -> Function -> Flow Graph Java.Expression
encodeFunction env dom cod fun = case fun of
    FunctionElimination elm -> withTrace ("elimination (" ++ show (eliminationVariant elm) ++ ")") $ do
      encodeElimination env Nothing dom cod elm
    FunctionLambda lam@(Lambda var _ body) -> withTrace ("lambda " ++ unName var) $ withLambda env lam $ \env2 -> do
      -- For curried functions in Java, we need to check if body is also a lambda
      -- If it is, encode it recursively to create nested Java lambdas
      case deannotateTerm body of
        TermFunction (FunctionLambda innerLam) -> do
          -- Body is another lambda - recursively encode it.
          -- Use `cod` (the codomain of the current lambda) directly as the body's type.
          -- This preserves canonical type variable names from the method's type scheme,
          -- avoiding fresh variable names that `typeOf` would create via instantiateTypeScheme.
          case deannotateType cod of
            TypeFunction (FunctionType dom' cod') -> do
              innerJavaLambda <- encodeFunction env2 dom' cod' (FunctionLambda innerLam)
              let lam' = javaLambda var innerJavaLambda
              -- Apply cast only if the type variables are safe (from method's formal params)
              applyCastIfSafe aliases (TypeFunction $ FunctionType dom cod) lam'
            _ -> fail $ "expected function type for lambda body, but got: " ++ show cod
        _ -> do
          -- Body is not a lambda - analyze and encode normally
          fs <- withTrace "analyze function body" $ analyzeJavaFunction env2 body
          let bindings = functionStructureBindings fs
              innerBody = functionStructureBody fs
              env3 = functionStructureEnvironment fs

          (bindingStmts, env4) <- bindingsToStatements env3 bindings
          jbody <- encodeTerm env4 innerBody

          lam' <- if L.null bindings
            then return $ javaLambda var jbody
            else do
              let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
              return $ javaLambdaFromBlock var $ Java.Block $ bindingStmts ++ [returnSt]

          -- Apply cast only if the type variables are safe (from method's formal params)
          applyCastIfSafe aliases (TypeFunction $ FunctionType dom cod) lam'
    FunctionPrimitive name -> do
      let Java.Identifier classWithApply = elementJavaIdentifier True False aliases name
      let suffix = "." ++ applyMethodName
      let className = take (length classWithApply - length suffix) classWithApply
      -- For single-arg functions, use a method reference like ClassName::apply
      -- For multi-arg functions, generate a curried lambda wrapper since generated
      -- static methods take uncurried parameters
      let arity = typeArity (TypeFunction $ FunctionType dom cod)
      if arity <= 1
        then return $ javaIdentifierToJavaExpression $ Java.Identifier $ className ++ "::" ++ applyMethodName
        else do
          -- Generate curried lambda: p0 -> p1 -> ... -> ClassName.apply(p0, p1, ...)
          let paramNames = [Name ("p" ++ show i) | i <- [0..arity-1]]
          let paramExprs = javaIdentifierToJavaExpression . variableToJavaIdentifier <$> paramNames
          let classId = Java.Identifier className
          let call = javaMethodInvocationToJavaExpression $
                methodInvocationStatic classId (Java.Identifier applyMethodName) paramExprs
          let buildCurried [] inner = inner
              buildCurried (p:ps) inner = javaLambda p (buildCurried ps inner)
          -- Add a cast to the curried function type
          jtype <- encodeType aliases S.empty (TypeFunction $ FunctionType dom cod)
          rt <- javaTypeToJavaReferenceType jtype
          return $ javaCastExpressionToJavaExpression $
            javaCastExpression rt (javaExpressionToJavaUnaryExpression $ buildCurried paramNames call)
    _ -> pure $ encodeLiteral $ LiteralString $
      "Unimplemented function variant: " ++ show (functionVariant fun)
  where
    aliases = javaEnvironmentAliases env

encodeLiteral :: Literal -> Java.Expression
encodeLiteral lit = case lit of
    LiteralBinary bs -> javaArrayCreation javaBytePrimitiveType (Just byteValues)
      where
        byteValues = javaArrayInitializer $ fmap toByteExpr $ B.unpack bs
        toByteExpr w = javaLiteralToJavaExpression $ Java.LiteralInteger $
          Java.IntegerLiteral $ fromIntegral w
    LiteralBoolean b -> litExp $ javaBoolean b
    LiteralFloat f -> case f of
        FloatValueBigfloat v -> javaConstructorCall
          (javaConstructorName (Java.Identifier "java.math.BigDecimal") Nothing)
          [encodeLiteral $ LiteralString $ Literals.showBigfloat v]
          Nothing
        FloatValueFloat32 v -> primCast (Java.PrimitiveTypeNumeric $ Java.NumericTypeFloatingPoint Java.FloatingPointTypeFloat) $ litExp $
              Java.LiteralFloatingPoint $ Java.FloatingPointLiteral $ realToFrac v
        FloatValueFloat64 v -> litExp $ Java.LiteralFloatingPoint $ Java.FloatingPointLiteral v
    LiteralInteger i -> case i of
        IntegerValueBigint v -> javaConstructorCall
          (javaConstructorName (Java.Identifier "java.math.BigInteger") Nothing)
          [encodeLiteral $ LiteralString $ Literals.showBigint v]
          Nothing
        IntegerValueInt8 v -> primCast (Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeByte) $ litExp $ integer $ fromIntegral v
        IntegerValueInt16 v -> primCast (Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeShort) $ litExp $ integer $ fromIntegral v
        IntegerValueInt32 v -> litExp $ integer $ fromIntegral v
        IntegerValueInt64 v -> primCast (Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeLong) $ litExp $ integer $ fromIntegral v
        IntegerValueUint8 v -> primCast (Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeShort) $ litExp $ integer $ fromIntegral v
        IntegerValueUint16 v -> litExp $ Java.LiteralCharacter $ fromIntegral v
        IntegerValueUint32 v -> primCast (Java.PrimitiveTypeNumeric $ Java.NumericTypeIntegral Java.IntegralTypeLong) $ litExp $ integer $ fromIntegral v
        IntegerValueUint64 v -> javaConstructorCall
          (javaConstructorName (Java.Identifier "java.math.BigInteger") Nothing)
          [encodeLiteral $ LiteralString $ Literals.showBigint (fromIntegral v)]
          Nothing
      where
        integer = Java.LiteralInteger . Java.IntegerLiteral
    LiteralString s -> litExp $ javaString s
  where
    litExp = javaLiteralToJavaExpression
    primCast pt expr = javaCastExpressionToJavaExpression $
      javaCastPrimitive pt (javaExpressionToJavaUnaryExpression expr)

-- Note: we use Java object types everywhere, rather than primitive types, as the latter cannot be used
--       to build function types, parameterized types, etc.
encodeLiteralType :: LiteralType -> Flow Graph Java.Type
encodeLiteralType lt = case lt of
    LiteralTypeBinary -> pure $ Java.TypeReference $ Java.ReferenceTypeArray $
      Java.ArrayType (Java.Dims [[]]) $ Java.ArrayType_VariantPrimitive javaBytePrimitiveType
    LiteralTypeBoolean -> simple "Boolean"
    LiteralTypeFloat ft -> case ft of
      FloatTypeBigfloat -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigDecimal"
      FloatTypeFloat32 -> simple "Float"
      FloatTypeFloat64 -> simple "Double"
    LiteralTypeInteger it -> case it of
      IntegerTypeBigint -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigInteger"
      IntegerTypeInt8 -> simple "Byte"
      IntegerTypeInt16 -> simple "Short"
      IntegerTypeInt32 -> simple "Integer"
      IntegerTypeInt64 -> simple "Long"
      IntegerTypeUint8 -> simple "Short"
      IntegerTypeUint16 -> simple "Character"
      IntegerTypeUint32 -> simple "Long"
      IntegerTypeUint64 -> pure $ javaRefType [] (Just $ javaPackageName ["java", "math"]) "BigInteger"
    LiteralTypeString -> simple "String"
    _ -> fail $ "unexpected literal type: " ++ show lt
  where
    simple n = pure $ javaRefType [] Nothing n

encodeNullaryConstant :: JavaEnvironment -> Type -> Function -> Flow Graph Java.Expression
encodeNullaryConstant env typ fun = case fun of
  -- For nullary primitives that return a value (not a function), call them directly
  -- Need to include type arguments for generic methods like Empty.<Name>apply()
  FunctionPrimitive name -> do
    let aliases = javaEnvironmentAliases env
    targs <- typeArgumentsFromReturnType aliases typ
    if L.null targs
      then do
        let header = Java.MethodInvocation_HeaderSimple $ Java.MethodName $ elementJavaIdentifier True False aliases name
        return $ javaMethodInvocationToJavaExpression $ Java.MethodInvocation header []
      else do
        -- Use qualified call with type arguments: ClassName.<Type>apply()
        let Java.Identifier fullName = elementJavaIdentifier True False aliases name
        -- Split "hydra.lib.sets.Empty.apply" into class "hydra.lib.sets.Empty" and method "apply"
        let parts = LS.splitOn "." fullName
        let className = Java.Identifier $ L.intercalate "." $ L.init parts
        let methodName = Java.Identifier $ L.last parts
        return $ javaMethodInvocationToJavaExpression $
          methodInvocationStaticWithTypeArgs className methodName targs []
  _ -> unexpected "nullary function" $ show fun
  where
    -- Extract type arguments from the return type (e.g., Set<Name> -> [Name])
    typeArgumentsFromReturnType :: Aliases -> Type -> Flow Graph [Java.TypeArgument]
    typeArgumentsFromReturnType aliases t = case deannotateType t of
      TypeSet st -> do
        jst <- encodeType aliases S.empty st >>= javaTypeToJavaReferenceType
        return [Java.TypeArgumentReference jst]
      TypeList lt -> do
        jlt <- encodeType aliases S.empty lt >>= javaTypeToJavaReferenceType
        return [Java.TypeArgumentReference jlt]
      TypeMaybe mt -> do
        jmt <- encodeType aliases S.empty mt >>= javaTypeToJavaReferenceType
        return [Java.TypeArgumentReference jmt]
      TypeMap (MapType kt vt) -> do
        jkt <- encodeType aliases S.empty kt >>= javaTypeToJavaReferenceType
        jvt <- encodeType aliases S.empty vt >>= javaTypeToJavaReferenceType
        return [Java.TypeArgumentReference jkt, Java.TypeArgumentReference jvt]
      _ -> return []

encodeTerm :: JavaEnvironment -> Term -> Flow Graph Java.Expression
encodeTerm env term0 = encodeInternal [] [] term0
  where
    aliases = javaEnvironmentAliases env
    tc = javaEnvironmentTypeContext env
    encode = encodeTerm env
    failAsLiteral msg = pure $ encodeLiteral $ LiteralString msg
    encodeInternal anns tyapps term = case term of
        TermAnnotated (AnnotatedTerm term' ann) -> encodeInternal (ann:anns) tyapps term'

        TermApplication app -> withTrace "encode application" $ encodeApplication env app

        TermEither et -> do
          targs <- takeTypeArgs "either" 2
          case et of
            Left term1 -> do
              expr <- encode term1
              return $ javaMethodInvocationToJavaExpression $
                methodInvocationStaticWithTypeArgs (Java.Identifier "hydra.util.Either") (Java.Identifier "left") targs [expr]
            Right term1 -> do
              expr <- encode term1
              return $ javaMethodInvocationToJavaExpression $
                methodInvocationStaticWithTypeArgs (Java.Identifier "hydra.util.Either") (Java.Identifier "right") targs [expr]

        TermFunction f -> withTrace ("encode function (" ++ show (functionVariant f) ++ ")") $ do
          -- Try to get type from annotations first (works better with hoisted terms)
          -- Fall back to typeOf only if annotation not available
          let combinedAnns = M.unions anns
          mt <- getType combinedAnns
          typ <- case mt of
            Just t -> pure t
            Nothing -> do
              -- Try to infer the type from the function structure before falling back to typeOf
              case tryInferFunctionType f of
                Just inferredType -> pure inferredType
                Nothing -> tryTypeOf "4" tc term0
          case deannotateType typ of
            TypeFunction (FunctionType dom cod) -> do
              encodeFunction env dom cod f
            _ -> encodeNullaryConstant env typ f

        TermLet lt -> withTrace "encode let as block" $ do
          -- Convert let bindings to local variable declarations in a block-bodied nullary lambda,
          -- then immediately invoke with .get(). This avoids eager evaluation of binding values
          -- (which the previous lambda-application encoding caused).
          -- Emits: ((Supplier<R>) (() -> { T x = e; return body; })).get()
          let bindings = letBindings lt
          let body = letBody lt
          if L.null bindings
            then encode body
            else do
              (bindingStmts, env2) <- bindingsToStatements env bindings
              jbody <- encodeTerm env2 body
              let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
              let block = Java.Block $ bindingStmts ++ [returnSt]
              let nullaryLambda = Java.ExpressionLambda $
                    Java.LambdaExpression (Java.LambdaParametersTuple []) (Java.LambdaBodyBlock block)
              -- Get the type of the TermLet expression for the Supplier<R> cast.
              -- First try annotations (most reliable), then fall back to type inference.
              let combinedAnns = M.unions anns
              let tc2 = javaEnvironmentTypeContext env2
              let aliases2 = javaEnvironmentAliases env2
              mt <- getType combinedAnns
              letType <- case mt of
                Just t -> pure t
                Nothing -> tryTypeOf "let-body" tc2 body
              jLetType <- encodeType aliases2 S.empty letType
              rt <- javaTypeToJavaReferenceType jLetType
              let supplierRt = Java.ReferenceTypeClassOrInterface $ Java.ClassOrInterfaceTypeClass $
                    javaClassType [rt] javaUtilFunctionPackageName "Supplier"
              let castExpr = javaCastExpressionToJavaExpression $
                    javaCastExpression supplierRt (javaExpressionToJavaUnaryExpression nullaryLambda)
              return $ javaMethodInvocationToJavaExpression $
                methodInvocation (Just $ Right $ javaExpressionToJavaPrimary castExpr) (Java.Identifier "get") []

        TermList els -> do
          jels <- CM.mapM encode els
          targs <- if L.null jels
            then takeTypeArgs "list" 1
            else pure []
          return $ javaMethodInvocationToJavaExpression $
            methodInvocationStaticWithTypeArgs (Java.Identifier "java.util.List") (Java.Identifier "of") targs jels

        TermLiteral l -> pure $ encodeLiteral l

        TermMap m -> do
          jkeys <- CM.mapM encode $ M.keys m
          jvals <- CM.mapM encode $ M.elems m
          let pairs = L.zip jkeys jvals
          let pairExprs = (\(k, v) -> javaMethodInvocationToJavaExpression $
                methodInvocationStatic (Java.Identifier "java.util.Map") (Java.Identifier "entry") [k, v]) <$> pairs
          targs <- if M.null m
            then takeTypeArgs "map" 2
            else pure []
          return $ javaMethodInvocationToJavaExpression $
            methodInvocationStaticWithTypeArgs (Java.Identifier "java.util.Map") (Java.Identifier "ofEntries") targs pairExprs

        TermMaybe mt -> case mt of
          Nothing -> do
            targs <- takeTypeArgs "maybe" 1
            return $ javaMethodInvocationToJavaExpression $
              methodInvocationStaticWithTypeArgs (Java.Identifier "hydra.util.Maybe") (Java.Identifier "nothing") targs []
          Just term1 -> do
            expr <- encode term1
            return $ javaMethodInvocationToJavaExpression $
              methodInvocationStatic (Java.Identifier "hydra.util.Maybe") (Java.Identifier "just") [expr]

        TermPair (t1, t2) -> do
          jterm1 <- encode t1
          jterm2 <- encode t2
          let tupleTypeName = "hydra.util.Tuple.Tuple2"
          -- Use type args from outer type applications for generic Tuple2 constructor
          mtargs <- if L.null tyapps
            then return Nothing
            else do
              rts <- CM.mapM javaTypeToJavaReferenceType tyapps
              return $ Just $ Java.TypeArgumentsOrDiamondArguments $ fmap Java.TypeArgumentReference rts
          return $ javaConstructorCall (javaConstructorName (Java.Identifier tupleTypeName) mtargs) [jterm1, jterm2] Nothing

        TermRecord (Record name fields) -> do
          fieldExprs <- CM.mapM encode (fieldTerm <$> fields)
          let consId = nameToJavaName aliases name
          -- Use type arguments from outer type applications to avoid raw generic constructors
          mtargs <- if L.null tyapps
            then return Nothing
            else do
              rts <- CM.mapM javaTypeToJavaReferenceType tyapps
              return $ Just $ Java.TypeArgumentsOrDiamondArguments $ fmap Java.TypeArgumentReference rts
          return $ javaConstructorCall (javaConstructorName consId mtargs) fieldExprs Nothing

        TermSet s -> do
          jels <- CM.mapM encode $ S.toList s
          if S.null s
            then do
              targs <- takeTypeArgs "set" 1
              return $ javaMethodInvocationToJavaExpression $
                methodInvocationStaticWithTypeArgs (Java.Identifier "java.util.Set") (Java.Identifier "of") targs []
            else do
              let prim = javaMethodInvocationToJavaPrimary $
                         methodInvocationStatic (Java.Identifier "java.util.stream.Stream") (Java.Identifier "of") jels
              let coll = javaMethodInvocationToJavaExpression $
                         methodInvocationStatic (Java.Identifier "java.util.stream.Collectors") (Java.Identifier "toSet") []
              return $ javaMethodInvocationToJavaExpression $
                methodInvocation (Just $ Right prim) (Java.Identifier "collect") [coll]

        TermTypeApplication (TypeApplicationTerm body atyp) -> do
          -- Type applications in Java may require casting to the appropriate type
          -- Try to get type from annotations first (works better with hoisted terms)
          let combinedAnns = M.unions anns
          mtyp <- getType combinedAnns
          typ <- case mtyp of
            Just t -> pure t
            Nothing -> tryTypeOf "5" tc term0
          let (innermostBody0, allTypeArgs0) = collectTypeApps0 body [atyp]
          -- Try to reconstruct the correct type from the innermost body's annotation
          -- and the type applications, since the annotation on the outer TermTypeApplication
          -- may have stale type variables (not updated by normalizeTypeVariablesInTerm)
          correctedTyp <- correctCastType innermostBody0 allTypeArgs0 typ
          jatyp <- encodeType aliases S.empty atyp
          -- Collect all type arguments from nested type applications
          let (innermostBody, allTypeArgs) = collectTypeApps body [atyp]
          -- When the innermost body is a variable that maps to a nullary static method,
          -- generate explicit type witnesses (e.g. Monads.<Graph>getState()) instead of
          -- a cast, which Java can't resolve for methods with unconstrained type params.
          case innermostBody of
            TermVariable varName -> do
              cls <- classifyDataReference varName
              case cls of
                JavaSymbolClassNullaryFunction -> do
                  let QualifiedName mns localName = qualifyName varName
                  case mns of
                    Just ns -> do
                      let classId = nameToJavaName aliases $ unqualifyName $ QualifiedName mns (elementsClassName ns)
                      let methodId = Java.Identifier $ sanitizeJavaName localName
                      filteredTypeArgs <- filterPhantomTypeArgs varName allTypeArgs
                      jTypeArgs <- CM.mapM (\t -> do
                        jt <- encodeType aliases S.empty t
                        rt <- javaTypeToJavaReferenceType jt
                        return $ Java.TypeArgumentReference rt) filteredTypeArgs
                      return $ javaMethodInvocationToJavaExpression $
                        methodInvocationStaticWithTypeArgs classId methodId jTypeArgs []
                    Nothing -> fallbackCast jatyp body correctedTyp
                JavaSymbolClassHoistedLambda arity -> do
                  let QualifiedName mns localName = qualifyName varName
                  case mns of
                    Just ns -> do
                      let classId = nameToJavaName aliases $ unqualifyName $ QualifiedName mns (elementsClassName ns)
                      let methodId = Java.Identifier $ sanitizeJavaName localName
                      filteredTypeArgs <- filterPhantomTypeArgs varName allTypeArgs
                      jTypeArgs <- CM.mapM (\t -> do
                        jt <- encodeType aliases S.empty t
                        rt <- javaTypeToJavaReferenceType jt
                        return $ Java.TypeArgumentReference rt) filteredTypeArgs
                      let paramNames = [Name ("p" ++ show i) | i <- [0..arity-1]]
                      let paramIds = variableToJavaIdentifier <$> paramNames
                      let paramExprs = javaIdentifierToJavaExpression <$> paramIds
                      let call = javaMethodInvocationToJavaExpression $
                            methodInvocationStaticWithTypeArgs classId methodId jTypeArgs paramExprs
                      let buildCurried [] inner = inner
                          buildCurried (p:ps) inner = javaLambda p (buildCurried ps inner)
                      return $ buildCurried paramNames call
                    Nothing -> fallbackCast jatyp body correctedTyp
                _ -> fallbackCast jatyp body correctedTyp
            _ -> fallbackCast jatyp body correctedTyp
            _ -> fallbackCast jatyp body correctedTyp
          where
            collectTypeApps t acc = case deannotateTerm t of
              TermTypeApplication (TypeApplicationTerm inner innerTyp) ->
                collectTypeApps inner (innerTyp : acc)
              other -> (other, acc)
            collectTypeApps0 t acc = case deannotateTerm t of
              TermTypeApplication (TypeApplicationTerm inner innerTyp) ->
                collectTypeApps0 inner (innerTyp : acc)
              _ -> (t, acc)
            -- Correct the cast type by building a substitution that maps old
            -- (stale) type variables to the correct (renamed) ones. We do this by
            -- comparing the type variables that appear in the annotation type with
            -- the actual type arguments from the TermTypeApplication chain.
            correctCastType innerBody typeArgs fallback = do
              case deannotateTerm innerBody of
                -- For pair terms with exactly 2 type args, reconstruct the pair type
                -- from the type args (which are correctly renamed by normalizeTypeVariablesInTerm)
                -- instead of using the annotation type (which may have stale variable names).
                TermPair _ | L.length typeArgs == 2 ->
                  return $ TypePair $ PairType (typeArgs !! 0) (typeArgs !! 1)
                _ -> return fallback
            fallbackCast jatyp body typ = do
              -- Also re-annotate the body with the corrected type so inner casts are correct
              let annotatedBody = setTermAnnotation key_type (Just $ EncodeCore.type_ typ) body
              jbody <- encodeInternal anns (jatyp:tyapps) annotatedBody
              jtype <- encodeType aliases S.empty typ
              rt <- javaTypeToJavaReferenceType jtype
              return $ javaCastExpressionToJavaExpression $
                javaCastExpression rt (javaExpressionToJavaUnaryExpression jbody)

        TermTypeLambda tl@(TypeLambda v body) -> withTypeLambda env tl $ \env2 -> do
          -- When entering a type lambda body, extract the body type from the forall
          -- and re-annotate the body with it, so nested encoding can find the type
          let combinedAnns = M.unions anns
          mtyp <- getType combinedAnns
          let annotatedBody = case mtyp of
                Just (TypeForall (ForallType _ bodyType)) ->
                  setTermAnnotation key_type (Just $ EncodeCore.type_ bodyType) body
                _ -> body
          encodeTerm env2 annotatedBody

        TermUnion (Injection name (Field (Name fname) v)) -> do
          let (Java.Identifier typeId) = nameToJavaName aliases name
          let consId = Java.Identifier $ typeId ++ "." ++ sanitizeJavaName (capitalize fname)
          -- Check if the field type is unit by looking up the union's schema
          fieldIsUnit <- isFieldUnitType name (Name fname)
          args <- if Schemas.isUnitTerm (deannotateTerm v) || fieldIsUnit
            then return []
            else do
              ex <- encode v
              return [ex]
          return $ javaConstructorCall (javaConstructorName consId Nothing) args Nothing

        TermVariable name -> encodeVariable env name

        TermUnit -> do
          -- Check the expected type to determine how to encode unit
          -- If the expected type is TypeUnit (mapped to Void), emit null
          -- Otherwise, the unit value is likely a union variant constructor, emit the proper class
          let combinedAnns = M.unions anns
          mtyp <- getType combinedAnns
          case mtyp of
            Just (TypeUnit) -> pure $ javaLiteralToJavaExpression Java.LiteralNull
            _ -> pure $ javaLiteralToJavaExpression Java.LiteralNull

        TermWrap (WrappedTerm tname arg) -> do
          jarg <- encode arg
          return $ javaConstructorCall (javaConstructorName (nameToJavaName aliases tname) Nothing) [jarg] Nothing

        _ -> failAsLiteral $ "Unimplemented term variant: " ++ show (termVariant term)
      where
        takeTypeArgs label n = if L.length tyapps < n
          then fail $ "needed " ++ show n ++ " type arguments for " ++ label ++ ", found " ++ show (L.length tyapps)
          else do
            rt <- CM.mapM javaTypeToJavaReferenceType $ L.take n tyapps
            return $ fmap Java.TypeArgumentReference rt
        -- Check if a union variant field's type is a unit type, by looking up the union type schema
        isFieldUnitType typeName fieldName = do
          g <- getState
          ix <- Schemas.graphToInferenceContext g
          let schemaTypes = inferenceContextSchemaTypes ix
          case M.lookup typeName schemaTypes of
            Nothing -> return False
            Just ts -> case deannotateType (typeSchemeType ts) of
              TypeUnion (RowType _ fields) ->
                return $ Y.maybe False (\ft -> Schemas.isUnitType (deannotateType (fieldTypeType ft))) $
                  L.find (\ft -> fieldTypeName ft == fieldName) fields
              _ -> return False

-- | Convert a list of bindings to Java block statements, handling recursive bindings
-- and performing topological sorting for correct declaration order.
bindingsToStatements :: JavaEnvironment -> [Binding] -> Flow Graph ([Java.BlockStatement], JavaEnvironment)
bindingsToStatements env bindings = if L.null bindings
    then return ([], envExtended)
    else do
      groups <- CM.mapM toDeclStatements sorted
      return (L.concat groups, envExtended)
  where
    aliases = javaEnvironmentAliases env
    tc = javaEnvironmentTypeContext env

    -- Flatten nested lets: if a binding's value is a TermLet, expand it into multiple bindings.
    -- After flattening, deduplicate binding names that collide with in-scope variables
    -- from enclosing scopes (to satisfy Java's lambda shadowing restriction).
    flattenedBindings = dedup (aliasesInScopeJavaVars aliases) $ L.concatMap flattenOne bindings
      where
        flattenOne (Binding name term mts) = case deannotateTerm term of
          TermLet (Let innerBindings body) -> flattenBindings innerBindings ++ [Binding name body mts]
          _ -> [Binding name term mts]
        flattenBindings bs = L.concatMap flattenOne bs

        -- Deduplicate: rename binding names that collide with inScope or earlier siblings
        dedup inScope [] = []
        dedup inScope (Binding name term mts : rest)
          | S.member name inScope =
              let newName = freshName name inScope
                  subst = M.singleton name newName
                  -- Only substitute in the terms that reference this binding:
                  -- the rest of the flattened list up to (and including) the parent binding
                  rest' = L.map (\(Binding n t ts') ->
                    Binding n (substituteVariables subst t) ts') rest
              in Binding newName term mts : dedup (S.insert newName inScope) rest'
          | otherwise = Binding name term mts : dedup (S.insert name inScope) rest

        freshName (Name base) avoid = L.head $ L.filter (`S.notMember` avoid)
          [Name (base ++ show i) | i <- [(2::Int)..]]

    -- Extend TypeContext with flattened bindings so they can reference each other
    tcExtended = extendTypeContextForLet bindingMetadata tc (Let flattenedBindings (TermVariable $ Name "dummy"))

    bindingVars = S.fromList (bindingName <$> flattenedBindings)

    -- Identify recursive bindings
    recursiveVars = S.fromList $ L.concat (ifRec <$> sorted)
      where
        ifRec names = case names of
          [name] -> case M.lookup name allDeps of
            Nothing -> []
            Just deps -> if S.member name deps then [name] else []
          _ -> names  -- Mutually recursive group

    -- Identify bindings that need thunking (lazy evaluation via Supplier).
    -- A binding needs thunking if it is:
    --   1. Not recursive (recursive bindings use AtomicReference)
    --   2. Contains a subterm that needs lazy evaluation (TermLet, references to thunked vars, etc.)
    --   3. Has zero arity (not a function — functions are already lazy)
    thunkedVars = S.fromList [bindingName b | b <- flattenedBindings,
      not (S.member (bindingName b) recursiveVars),
      needsThunking (bindingTerm b),
      not (isFunctionType b)]
    isFunctionType (Binding _ _ (Just ts)) = case deannotateType (typeSchemeType ts) of
      TypeFunction _ -> True
      TypeForall (ForallType _ body) -> case deannotateType body of
        TypeFunction _ -> True
        _ -> False
      _ -> False
    isFunctionType (Binding _ term Nothing) = case deannotateTerm term of
      TermFunction _ -> True
      _ -> False
    -- Check if a term structurally needs lazy evaluation.
    -- Unlike isComplexTerm, this does NOT follow variable references — only checks
    -- for direct structural complexity (TermLet, TermTypeApplication, TermTypeLambda).
    needsThunking t = case deannotateTerm t of
      TermLet _ -> True
      TermTypeApplication _ -> True
      TermTypeLambda _ -> True
      _ -> L.foldl (\b st -> b || needsThunking st) False $ subterms t

    -- Create environment with recursive vars marked in aliases and binding names in scope
    aliasesExtended = aliases {
      aliasesRecursiveVars = S.union (aliasesRecursiveVars aliases) recursiveVars,
      aliasesInScopeJavaVars = S.union (aliasesInScopeJavaVars aliases) bindingVars,
      aliasesThunkedVars = S.union (aliasesThunkedVars aliases) thunkedVars }
    envExtended = env {
      javaEnvironmentTypeContext = tcExtended,
      javaEnvironmentAliases = aliasesExtended
    }

    -- Build dependency graph
    allDeps = M.fromList (toDeps <$> flattenedBindings)
      where
        toDeps (Binding key value _) = (key, S.filter (\n -> S.member n bindingVars) $ freeVariablesInTerm value)

    -- Topological sort for correct declaration order
    sorted = topologicalSortComponents (toDeps <$> M.toList allDeps)
      where
        toDeps (key, deps) = (key, S.toList deps)

    -- Convert a group of bindings to statements
    toDeclStatements names = do
      inits <- Y.catMaybes <$> CM.mapM toDeclInit names
      impls <- CM.mapM toDeclStatement names
      return $ inits ++ impls

    -- Initialize recursive bindings with AtomicReference
    toDeclInit name = if S.member name recursiveVars
      then do
        let binding = L.head $ L.filter (\b -> bindingName b == name) flattenedBindings
        let value = bindingTerm binding
        -- Use the binding's type scheme if available, otherwise infer from the term
        typ <- case bindingType binding of
          Just ts -> pure $ typeSchemeType ts
          Nothing -> tryTypeOf "6" tcExtended value
        jtype <- encodeType aliasesExtended S.empty typ
        let id = variableToJavaIdentifier name
        let pkg = javaPackageName ["java", "util", "concurrent", "atomic"]
        let arid = Java.Identifier "java.util.concurrent.atomic.AtomicReference"
        let aid = Java.AnnotatedIdentifier [] arid
        rt <- javaTypeToJavaReferenceType jtype
        let targs = typeArgsOrDiamond [Java.TypeArgumentReference rt]
        let ci = Java.ClassOrInterfaceTypeToInstantiate [aid] (Just targs)
        let body = javaConstructorCall ci [] Nothing
        let artype = javaRefType [rt] (Just pkg) "AtomicReference"
        return $ Just $ variableDeclarationStatement aliasesExtended artype id body
      else pure Nothing

    -- Declare or set binding value
    toDeclStatement name = do
      let binding = L.head $ L.filter (\b -> bindingName b == name) flattenedBindings
      let value = bindingTerm binding
      -- Use the binding's type scheme if available, otherwise infer from the term
      typ <- case bindingType binding of
        Just ts -> pure $ typeSchemeType ts
        Nothing -> tryTypeOf "7" tcExtended value
      jtype <- encodeType aliasesExtended S.empty typ
      let id = variableToJavaIdentifier name
      -- Annotate the value with its type so that nested encoding can find it
      let annotatedValue = setTermAnnotation key_type (Just $ EncodeCore.type_ typ) value
      rhs <- encodeTerm envExtended annotatedValue
      if S.member name recursiveVars
        then return $ Java.BlockStatementStatement $ javaMethodInvocationToJavaStatement $
          methodInvocation (Just $ Left $ Java.ExpressionName Nothing id) (Java.Identifier setMethodName) [rhs]
        else if S.member name thunkedVars
          then do
            -- Wrap in Lazy<T> for memoized lazy evaluation (like Python's @lru_cache(1))
            rt <- javaTypeToJavaReferenceType jtype
            let lazyType = javaRefType [rt] hydraUtilPackageName "Lazy"
            let lambdaBody = Java.LambdaBodyExpression rhs
            let supplierLambda = Java.ExpressionLambda $
                  Java.LambdaExpression (Java.LambdaParametersTuple []) lambdaBody
            let targs = typeArgsOrDiamond [Java.TypeArgumentReference rt]
            let lazyExpr = javaConstructorCall (javaConstructorName (Java.Identifier "hydra.util.Lazy") (Just targs)) [supplierLambda] Nothing
            return $ variableDeclarationStatement aliasesExtended lazyType id lazyExpr
          else return $ variableDeclarationStatement aliasesExtended jtype id rhs

-- Lambdas cannot (in general) be turned into top-level constants, as there is no way of declaring type parameters for constants
-- These functions must be capable of handling various combinations of let and lambda terms:
-- * Plain lambdas such as \x y -> x + y + 42
-- * Lambdas with nested let terms, such as \x y -> let z = x + y in z + 42
-- * Let terms with nested lambdas, such as let z = 42 in \x y -> x + y + z
encodeTermDefinition :: JavaEnvironment -> TermDefinition -> Flow Graph Java.InterfaceMemberDeclaration
encodeTermDefinition env (TermDefinition name term0 ts) = withTrace ("encode term definition \"" ++ unName name ++ "\"") $ do
    -- Unshadow variables to avoid Java lambda shadowing restrictions
    let term = unshadowVariables term0
    fs <- withTrace "analyze function term for term assignment" $ analyzeJavaFunction env term
    -- Use type scheme variables as the authoritative source for type parameters.
    -- This is critical for hoisted polymorphic bindings where TermTypeLambda nodes
    -- may have been stripped but the TypeScheme still contains the type variables.
    -- Filter out qualified names from type scheme variables - these are type names, not type variables
    -- Type variables should be simple names like "t0", "s", "x", not qualified names like "Hydra.compute.Flow"
    let isSimpleName (Name n) = not ('.' `elem` n)
        schemeVars = L.filter isSimpleName $ typeSchemeVariables ts
        termVars = functionStructureTypeParams fs
        -- Filter scheme vars to only those actually used in the scheme type.
        -- Inference can over-generalize hoisted bindings, adding phantom type variables
        -- that appear in the scheme's variable list but not in the actual type.
        schemeTypeVars = collectTypeVars (typeSchemeType ts)
        usedSchemeVars = L.filter (\v -> S.member v schemeTypeVars) schemeVars
        tparams = if L.null usedSchemeVars then termVars else usedSchemeVars
        params = functionStructureParams fs
        bindings = functionStructureBindings fs
        body = functionStructureBody fs
        doms = functionStructureDomains fs
        env2 = functionStructureEnvironment fs
    -- Derive the codomain from the TypeScheme, which is authoritative.
    -- We avoid using the body-inferred codomain (functionStructureCodomain) because
    -- the type checker treats free type variables as wildcards, which can pick the wrong
    -- branch type in case expressions (e.g., returning (t0, X) instead of (t1, X)).
    let schemeType = typeSchemeType ts
        numParams = L.length params
        -- Peel domain types from the scheme type to get individual canonical domains
        peelDomainsAndCod n t
          | n <= 0 = ([], t)
          | otherwise = case deannotateType t of
              TypeFunction (FunctionType d c) -> let (ds, cod') = peelDomainsAndCod (n - 1) c in (d:ds, cod')
              _ -> ([], t)  -- Can't peel further
        (schemeDoms, cod) = peelDomainsAndCod numParams schemeType
        schemeVarSet = S.fromList tparams
    -- Build a substitution by walking the body and matching annotation types (which have
    -- stale fresh variable names) against lambda domain types (which are normalized).
    typeVarSubst <- if L.null tparams
      then return M.empty
      else do
        buildSubstFromAnnotations schemeVarSet term
    -- Fix over-generalized type variables in the scheme type.
    -- After hoisting, inference may create separate type variables (e.g. T0, T1) for what
    -- should be the same type. Detect "accumulator pattern" unification: if two type vars
    -- appear as the first element of a pair return type in different function parameters
    -- with the same input type, they should be unified.
    let overgenSubst = detectAccumulatorUnification schemeDoms cod tparams
        -- Extract the Name->Name portion for aliasesTypeVarSubst (variable-to-variable only)
        overgenVarSubst = M.mapMaybe (\t -> case t of TypeVariable n -> Just n; _ -> Nothing) overgenSubst
        fixedCod = if M.null overgenSubst then cod
                   else substituteTypeVarsWithTypes overgenSubst cod
        fixedDoms = if M.null overgenSubst then schemeDoms
                   else fmap (substituteTypeVarsWithTypes overgenSubst) schemeDoms
        fixedTparams = if M.null overgenSubst then tparams
                   else L.filter (\v -> not $ M.member v overgenSubst) tparams
    let constraints = Y.fromMaybe M.empty (typeSchemeConstraints ts)
        jparams = fmap (toParam constraints) fixedTparams
        -- Update aliases to include method type parameters in scope
        -- This allows inner binding types to properly handle type variables
        aliases2base = javaEnvironmentAliases env2
        -- Also add lambda params (including hoisted captures) to aliasesLambdaVars
        -- This ensures that references to captured functions use .apply() instead of static calls
        -- Compute trusted type variables: those that appear in the method's formal parameter types
        -- and codomain. Lambda casts should only use these variables.
        trustedVars = S.unions $ fmap collectTypeVars (fixedDoms ++ [fixedCod])
        fixedSchemeVarSet = S.fromList fixedTparams
        aliases2 = aliases2base {
          aliasesInScopeTypeParams = fixedSchemeVarSet,
          aliasesLambdaVars = S.union (aliasesLambdaVars aliases2base) (S.fromList params),
          aliasesTypeVarSubst = M.union overgenVarSubst typeVarSubst,
          aliasesTrustedTypeVars = S.intersection trustedVars fixedSchemeVarSet,
          aliasesMethodCodomain = Just fixedCod }
        env2WithTypeParams = env2 { javaEnvironmentAliases = aliases2 }

    -- Convert bindings to Java block statements
    (bindingStmts, env3) <- bindingsToStatements env2WithTypeParams bindings

    -- Apply overgenSubst to the body's type annotations so that intermediate lambda
    -- casts use the corrected types rather than stale over-generalized variables.
    body' <- if M.null overgenSubst then return body
             else applyOvergenSubstToTermAnnotations overgenSubst body

    -- Annotate the body with its type so that nested encoding can find it.
    -- For applications where the LHS is a case statement, also annotate the LHS with
    -- the correct function type using the TypeScheme's codomain. This fixes type variable
    -- mismatches where the type checker picks the wrong branch type for the codomain.
    -- Also propagate expected types through application chains: for f(a1)(a2)...
    -- annotate f with arg1_type -> arg2_type -> ... -> resultType.
    let annotateAppLhs appLhs = case deannotateTerm appLhs of
          TermFunction (FunctionElimination (EliminationUnion cs)) ->
            let dom = Schemas.nominalApplication (caseStatementTypeName cs) []
                funType = TypeFunction $ FunctionType dom fixedCod
            in setTermAnnotation key_type (Just $ EncodeCore.type_ funType) appLhs
          _ -> appLhs
        -- Collect the domain annotations from a chain of nested lambdas.
        -- Returns (domains, body) where domains are the annotated domain types.
        collectLambdaDomains :: Term -> ([Type], Term)
        collectLambdaDomains t = case deannotateTerm t of
          TermFunction (FunctionLambda (Lambda _ (Just dom) innerBody)) ->
            let (ds, b) = collectLambdaDomains innerBody in (dom:ds, b)
          _ -> ([], t)
        -- For an application chain f(a1)(a2)...(aN) with known result type R,
        -- annotate f with its expected type by examining f's lambda structure.
        -- If f is a lambda with M params and N <= M args are applied,
        -- f's type = dom(p1) -> ... -> dom(pM) -> bodyReturnType
        -- where bodyReturnType is obtained by peeling (M-N) domains from R.
        propagateTypesInAppChain :: Type -> Term -> Term
        propagateTypesInAppChain resultType t =
          -- Flatten the application chain
          let (args, fun) = flattenApps t []
              -- If fun is a lambda, compute its type from domain annotations
              lambdaDoms = fst $ collectLambdaDomains fun
              nArgs = L.length args
              nLambdaDoms = L.length lambdaDoms
          in if nLambdaDoms > 0 && nArgs > 0
            then
              -- Compute bodyReturnType by peeling the excess lambda domains from resultType.
              -- resultType = dom(v_{nArgs+1}) -> ... -> dom(v_M) -> bodyReturnType
              -- So peel (M-N) function types from resultType to get bodyReturnType.
              let peelN n typ
                    | n <= 0 = typ
                    | otherwise = case deannotateType typ of
                        TypeFunction (FunctionType _ c) -> peelN (n - 1) c
                        _ -> typ
                  bodyRetType = peelN (nLambdaDoms - nArgs) resultType
                  -- Build the full lambda type: dom(v1) -> dom(v2) -> ... -> dom(vM) -> bodyRetType
                  funType = L.foldr (\d c -> TypeFunction $ FunctionType d c) bodyRetType lambdaDoms
                  -- Now annotate f and rebuild the application chain with correct intermediate types
                  annotatedFun = setTermAnnotation key_type (Just $ EncodeCore.type_ funType) fun
                  -- Build intermediate application types by peeling domains from funType
                  -- After applying arg_i, the remaining type is dom(v_{i+2}) -> ... -> bodyRetType
                  rebuildApps f [] _ = f
                  rebuildApps f (arg:rest) fType = case deannotateType fType of
                    TypeFunction (FunctionType _ remainingType) ->
                      let app = TermApplication $ Application f arg
                          annotatedApp = setTermAnnotation key_type (Just $ EncodeCore.type_ remainingType) app
                      in rebuildApps annotatedApp rest remainingType
                    _ -> L.foldl (\acc a -> TermApplication $ Application acc a) f (arg:rest)
              in rebuildApps annotatedFun args funType
            else
              -- Not a lambda function or no args - fall back to simple annotation
              case deannotateTerm t of
                TermApplication (Application lhs rhs) ->
                  setTermAnnotation key_type (Just $ EncodeCore.type_ resultType) $
                    TermApplication $ Application (annotateAppLhs lhs) rhs
                _ -> setTermAnnotation key_type (Just $ EncodeCore.type_ resultType) t
        flattenApps t acc = case deannotateTerm t of
          TermApplication (Application lhs rhs) -> flattenApps lhs (rhs:acc)
          _ -> (acc, t)
        annotatedBody = propagateTypesInAppChain fixedCod body'

    if False -- Constant fields disabled: use nullary methods to avoid forward-reference NPEs
      then do -- Special case: constant field (disabled)
        jtype <- Java.UnannType <$> encodeType aliases2 S.empty fixedCod
        jbody <- encodeTerm env3 annotatedBody
        let mods = []
        let var = javaVariableDeclarator (javaVariableName name) $ Just $ Java.VariableInitializerExpression jbody
        return $ Java.InterfaceMemberDeclarationConstant $ Java.ConstantDeclaration mods jtype [var]
      else do -- General case: method (possibly nullary)
        jformalParams <- mapM (\(dom, param) -> do
            jdom <- encodeType aliases2 S.empty dom
            return $ javaTypeToJavaFormalParameter jdom (Name $ unName param)
          ) (L.zip fixedDoms params)
        result <- javaTypeToJavaResult <$> encodeType aliases2 S.empty fixedCod
        jbody <- encodeTerm env3 annotatedBody
        let mods = [Java.InterfaceMethodModifierStatic]
        let returnSt = Java.BlockStatementStatement $ javaReturnStatement $ Just jbody
        return $ interfaceMethodDeclaration mods jparams jname jformalParams result (Just $ bindingStmts ++ [returnSt])
  where
    tc = javaEnvironmentTypeContext env
    jname = sanitizeJavaName $ decapitalize $ localNameOf name
    toParam constraints name@(Name v) = Java.TypeParameter [] (javaTypeIdentifier $ capitalize v) bound
      where
        bound = Nothing

encodeType :: Aliases -> S.Set Name -> Type -> Flow Graph Java.Type
encodeType aliases boundVars t =  case deannotateType t of
    TypeApplication (ApplicationType lhs rhs) -> do
      jlhs <- encode lhs
      jrhs <- encode rhs >>= javaTypeToJavaReferenceType
      addJavaTypeParameter jrhs jlhs
    TypeFunction (FunctionType dom cod) -> do
      jdom <- encode dom >>= javaTypeToJavaReferenceType
      jcod <- encode cod >>= javaTypeToJavaReferenceType
      return $ javaRefType [jdom, jcod] javaUtilFunctionPackageName "Function"
    TypeForall (ForallType v body) -> do
      jbody <- encodeType aliases (S.insert v boundVars) body
      addJavaTypeParameter (javaTypeVariable $ unName v) jbody
    TypeList et -> do
      jet <- encode et
      if listsAsArrays
        then toJavaArrayType jet
        else do
          rt <- javaTypeToJavaReferenceType jet
          return $ javaRefType [rt] javaUtilPackageName "List"
    TypeLiteral lt -> encodeLiteralType lt
    TypeEither (EitherType lt rt) -> do
      jlt <- encode lt >>= javaTypeToJavaReferenceType
      jrt <- encode rt >>= javaTypeToJavaReferenceType
      return $ javaRefType [jlt, jrt] hydraUtilPackageName "Either"
    TypeMap (MapType kt vt) -> do
      jkt <- encode kt >>= javaTypeToJavaReferenceType
      jvt <- encode vt >>= javaTypeToJavaReferenceType
      return $ javaRefType [jkt, jvt] javaUtilPackageName "Map"
    TypePair (PairType first second) -> do
      jfirst <- encode first >>= javaTypeToJavaReferenceType
      jsecond <- encode second >>= javaTypeToJavaReferenceType
      return $ javaRefType [jfirst, jsecond] hydraUtilPackageName "Tuple.Tuple2"
    TypeUnit -> unit
    TypeRecord (RowType _Unit []) -> unit
    TypeRecord (RowType name _) -> pure $
      Java.TypeReference $ nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) name Nothing
    TypeMaybe ot -> do
      jot <- encode ot >>= javaTypeToJavaReferenceType
      return $ javaRefType [jot] hydraUtilPackageName "Maybe"
    TypeSet st -> do
      jst <- encode st >>= javaTypeToJavaReferenceType
      return $ javaRefType [jst] javaUtilPackageName "Set"
    TypeUnion (RowType name _) -> pure $
      Java.TypeReference $ nameToJavaReferenceType aliases True (javaTypeArgumentsForType t) name Nothing
    TypeVariable name0 -> do
      -- Apply type variable substitution (maps fresh inference vars to canonical scheme vars)
      let name = Y.fromMaybe name0 $ M.lookup name0 (aliasesTypeVarSubst aliases)
      -- Check if this is a typedef (type alias) that should be resolved transparently
      resolved <- resolveIfTypedef name
      case resolved of
        Just resolvedType -> encode resolvedType
        Nothing -> pure $ forReference name
    TypeWrap (WrappedType name _) -> pure $ forReference name
    _ -> fail $ "can't encode unsupported type in Java: " ++ show t
  where
    inScopeTypeParams = aliasesInScopeTypeParams aliases
    -- A type variable is in scope if:
    -- 1. It's explicitly bound (from a forall in the type itself), OR
    -- 2. It's a method type parameter (in aliasesInScopeTypeParams)
    isInScope name = S.member name boundVars || S.member name inScopeTypeParams
    forReference name = if isInScope name
        then variableReference name
        -- Lambda-bound heuristic for backward compatibility
        else if isLambdaBoundVariable name
        then variableReference name
        -- Unresolved type inference variables (like t34403) become Object in Java
        else if isUnresolvedInferenceVar name
        then objectReference
        else nameReference name
    nameReference name = Java.TypeReference $ nameToJavaReferenceType aliases True [] name Nothing
    variableReference name = Java.TypeReference $ javaTypeVariable $ unName name
    objectReference = Java.TypeReference $ Java.ReferenceTypeClassOrInterface $
      Java.ClassOrInterfaceTypeClass $ javaClassType [] javaLangPackageName "Object"
    encode = encodeType aliases boundVars
    -- Resolve a TypeVariable name if it refers to a typedef (simple type alias like Vertex = int32).
    -- Returns Nothing for records, unions, wraps, polymorphic types, or unknown names.
    resolveIfTypedef name
      | isInScope name = return Nothing
      | isLambdaBoundVariable name = return Nothing
      | otherwise = do
          g <- getState
          ix <- Schemas.graphToInferenceContext g
          let schemaTypes = inferenceContextSchemaTypes ix
          case M.lookup name schemaTypes of
            Nothing -> return Nothing
            Just ts
              | not (L.null (typeSchemeVariables ts)) -> return Nothing  -- polymorphic, not a simple typedef
              | otherwise -> case deannotateType (typeSchemeType ts) of
                  TypeRecord _ -> return Nothing
                  TypeUnion _ -> return Nothing
                  TypeWrap _ -> return Nothing
                  _ -> return $ Just (typeSchemeType ts)
    unit = return $ javaRefType [] javaLangPackageName "Void"

encodeTypeDefinition :: Java.PackageDeclaration -> Aliases -> TypeDefinition -> Flow Graph (Name, Java.CompilationUnit)
encodeTypeDefinition pkg aliases (TypeDefinition name typ) = do
    decl <- typeDefToClassDecl aliases name typ
    return (name, Java.CompilationUnitOrdinary $ Java.OrdinaryCompilationUnit (Just pkg) imports [decl])
  where
    imports = if serializable
      then [Java.ImportDeclarationSingleType $ Java.SingleTypeImportDeclaration $ javaTypeName $ Java.Identifier "java.io.Serializable"]
      else []
    typeDefToClassDecl aliases name typ = do
      decl <- toClassDecl False serializable aliases [] name typ
      comment <- getTypeDescription typ
      return $ Java.TypeDeclarationWithComments (Java.TypeDeclarationClass decl) comment
    serializable = isSerializableType typ
    isSerializableType typ = case deannotateType typ of
      TypeRecord _ -> True
      TypeUnion _ -> True
      TypeWrap _ -> True
      TypeForall (ForallType _ body) -> isSerializableType body
      _ -> False

encodeVariable :: JavaEnvironment -> Name -> Flow Graph Java.Expression
encodeVariable env name =
    if S.member name (aliasesBranchVars aliases)
    then return $ javaFieldAccessToJavaExpression $ Java.FieldAccess
      (Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary $ javaIdentifierToJavaExpression jid)
      (javaIdentifier valueFieldName)
    -- Check if this is the special visitor instance value variable (instance_value)
    else if name == Name (instanceName ++ "_" ++ valueFieldName) && isRecursiveVariable aliases name
    then do
      -- Emit instance.value field access
      let instanceExpr = javaIdentifierToJavaExpression $ javaIdentifier instanceName
      return $ javaFieldAccessToJavaExpression $ Java.FieldAccess
        (Java.FieldAccess_QualifierPrimary $ javaExpressionToJavaPrimary instanceExpr)
        (javaIdentifier valueFieldName)
    else if isRecursiveVariable aliases name && not (isLambdaBoundIn name (aliasesLambdaVars aliases))
    then return $ javaMethodInvocationToJavaExpression $
      methodInvocation (Just $ Left $ Java.ExpressionName Nothing jid) (Java.Identifier getMethodName) []
    -- Thunked variables (wrapped in Supplier for lazy evaluation) use .get()
    else if S.member name (aliasesThunkedVars aliases) && not (isLambdaBoundIn name (aliasesLambdaVars aliases))
    then return $ javaMethodInvocationToJavaExpression $
      methodInvocation (Just $ Left $ Java.ExpressionName Nothing jid) (Java.Identifier getMethodName) []
    -- Lambda-bound variables (including hoisted captures with qualified names) use sanitized names
    -- Use isLambdaBoundIn to match both exact names and local parts of qualified names
    -- IMPORTANT: When the parameter was declared with a qualified name like
    -- "hydra/unification.Unification.unifyTypeConstraints", the Java parameter name becomes
    -- "hydra_unification_unifyTypeConstraints" (sanitized full name). So we must use the FULL name
    -- (not the local name) when encoding the reference, to match the parameter declaration.
    -- If the name is in lambdaVars directly, use it as-is; otherwise if only the local part matches,
    -- we need to find the actual parameter name that was declared.
    else if isLambdaBoundIn name (aliasesLambdaVars aliases)
    then do
      -- Find the actual name from lambdaVars that matches this reference
      let actualName = findMatchingLambdaVar name (aliasesLambdaVars aliases)
      return $ javaIdentifierToJavaExpression $ variableToJavaIdentifier actualName
    else do
      cls <- classifyDataReference name
      case cls of
        JavaSymbolClassHoistedLambda arity -> do
          -- Generate curried lambda wrapper for hoisted method without type args
          let paramNames = [Name ("p" ++ show i) | i <- [0..arity-1]]
          let paramExprs = javaIdentifierToJavaExpression . variableToJavaIdentifier <$> paramNames
          let call = javaMethodInvocationToJavaExpression $
                methodInvocation Nothing (elementJavaIdentifier False False aliases name) paramExprs
          let buildCurried [] inner = inner
              buildCurried (p:ps) inner = javaLambda p (buildCurried ps inner)
          let lam = buildCurried paramNames call
          -- Try to cast the lambda to the function's curried type to help Java's type inference
          mel <- dereferenceElement name
          case mel of
            Just el | Just ts <- bindingType el -> do
              let typ = typeSchemeType ts
              jtype <- encodeType aliases S.empty typ
              rt <- javaTypeToJavaReferenceType jtype
              return $ javaCastExpressionToJavaExpression $
                javaCastExpression rt (javaExpressionToJavaUnaryExpression lam)
            _ -> return lam
        _ -> return $ case cls of
          JavaSymbolLocalVariable -> javaIdentifierToJavaExpression $ elementJavaIdentifier False False aliases name
          JavaSymbolClassConstant -> javaIdentifierToJavaExpression $ elementJavaIdentifier False False aliases name
          JavaSymbolClassNullaryFunction -> javaMethodInvocationToJavaExpression $
            methodInvocation Nothing (elementJavaIdentifier False False aliases name) []
          JavaSymbolClassUnaryFunction -> javaIdentifierToJavaExpression $ elementJavaIdentifier False True aliases name
  where
    aliases = javaEnvironmentAliases env
    jid = javaIdentifier $ unName name


fieldTypeToFormalParam aliases (FieldType fname ft) = do
  jt <- encodeType aliases S.empty ft
  return $ javaTypeToJavaFormalParameter jt fname

functionCall :: JavaEnvironment -> Bool -> Name -> [Term] -> [Type] -> Flow Graph Java.Expression
functionCall env isPrim name args typeApps = do
    -- When there are no arguments and it's a primitive, use a method reference instead of calling .apply()
    if isPrim && L.null args && not isLambdaBound
    then do
      -- Generate method reference like ClassName::apply
      let Java.Identifier classWithApply = elementJavaIdentifier True False aliases name
      let suffix = "." ++ applyMethodName
      let className = take (length classWithApply - length suffix) classWithApply
      return $ javaIdentifierToJavaExpression $ Java.Identifier $ className ++ "::" ++ applyMethodName
    else do
      jargs0 <- CM.mapM (encodeTerm env) args
      let (jargs, mMethodOverride) = wrapLazyArguments name jargs0
      -- Check if this is a local variable OR a lambda-bound variable (from hoisted captures)
      -- Use isLambdaBoundIn to match both exact names and local parts of qualified names
      if isLocalVariable name || isLambdaBound
        then do
          -- Local/lambda-bound variables hold curried functions, so apply arguments one at a time
          -- Example: joinOne.apply(arg1).apply(arg2) instead of joinOne.apply(arg1, arg2)
          baseExpr <- encodeVariable env name
          return $ L.foldl' applySingle baseExpr jargs
        else do
          -- Module-level functions (both primitives and generated) take all args directly
          -- Include type witnesses from type applications to help Java infer return-only type params.
          -- If no type applications are present, check if the method has type parameters that
          -- can't be inferred from arguments (return-only type params). If so, add witnesses
          -- from the method's type scheme using the in-scope type variables.
          let effectiveTypeApps = typeApps
          let overrideMethodName jid = case mMethodOverride of
                Nothing -> jid
                Just m -> let Java.Identifier s = jid
                  in Java.Identifier $ reverse (drop (length applyMethodName) (reverse s)) ++ m
          if L.null effectiveTypeApps
            then do
              let header = Java.MethodInvocation_HeaderSimple $ Java.MethodName $ overrideMethodName $ elementJavaIdentifier isPrim False aliases name
              return $ javaMethodInvocationToJavaExpression $ Java.MethodInvocation header jargs
            else do
              let QualifiedName mns localName = qualifyName name
              case mns of
                Just ns -> do
                  let classId = nameToJavaName aliases $ unqualifyName $ QualifiedName mns (elementsClassName ns)
                  let methodId = if isPrim
                        then overrideMethodName $ Java.Identifier $ (Java.unIdentifier $ nameToJavaName aliases $ unqualifyName $ QualifiedName mns (capitalize localName)) ++ "." ++ applyMethodName
                        else Java.Identifier $ sanitizeJavaName localName
                  jTypeArgs <- CM.mapM (\t -> do
                    jt <- encodeType aliases S.empty t
                    rt <- javaTypeToJavaReferenceType jt
                    return $ Java.TypeArgumentReference rt) effectiveTypeApps
                  return $ javaMethodInvocationToJavaExpression $
                    methodInvocationStaticWithTypeArgs classId methodId jTypeArgs jargs
                Nothing -> do
                  let header = Java.MethodInvocation_HeaderSimple $ Java.MethodName $ overrideMethodName $ elementJavaIdentifier isPrim False aliases name
                  return $ javaMethodInvocationToJavaExpression $ Java.MethodInvocation header jargs
  where
    aliases = javaEnvironmentAliases env
    isLambdaBound = isLambdaBoundIn name (aliasesLambdaVars aliases)
    applySingle exp jarg = javaMethodInvocationToJavaExpression $
      methodInvocation (Just $ Right $ javaExpressionToJavaPrimary exp) (Java.Identifier applyMethodName) [jarg]

-- | For primitives requiring lazy evaluation, wrap branch arguments in Supplier lambdas
-- and return a replacement method name. Java eagerly evaluates all method arguments, so
-- ifElse branches must be wrapped in () -> expr and called via IfElse.lazy() to ensure
-- only the chosen branch is evaluated. This mirrors the Python coder's wrapLazyArguments.
wrapLazyArguments :: Name -> [Java.Expression] -> ([Java.Expression], Maybe String)
wrapLazyArguments name args
  | name == Name "hydra.lib.logic.ifElse" && length args == 3 =
      ([args !! 0, wrapInSupplierLambda (args !! 1), wrapInSupplierLambda (args !! 2)], Just "lazy")
  | otherwise = (args, Nothing)
  where
    wrapInSupplierLambda expr = Java.ExpressionLambda $
      Java.LambdaExpression (Java.LambdaParametersTuple []) (Java.LambdaBodyExpression expr)

getCodomain :: M.Map Name Term -> Flow Graph Type
getCodomain ann = functionTypeCodomain <$> getFunctionType ann

getFunctionType :: M.Map Name Term -> Flow Graph FunctionType
getFunctionType ann = do
  mt <- getType ann
  case mt of
    Nothing -> fail "type annotation is required for function and elimination terms in Java"
    Just t -> case t of
      TypeFunction ft -> return ft
      _ -> unexpected "function type (3)" $ show t

innerClassRef :: Aliases -> Name -> String -> Java.Identifier
innerClassRef aliases name local = Java.Identifier $ id ++ "." ++ local
  where
    Java.Identifier id = nameToJavaName aliases name

insertBranchVar :: Name -> JavaEnvironment -> JavaEnvironment
insertBranchVar name env = env {javaEnvironmentAliases = aliases {aliasesBranchVars = S.insert name (aliasesBranchVars aliases)}}
  where
    aliases = javaEnvironmentAliases env

serializableTypes :: Bool -> [Java.InterfaceType]
serializableTypes isSer = if isSer then [javaSerializableType] else []
  where
    javaSerializableType = Java.InterfaceType $
      Java.ClassType [] Java.ClassTypeQualifierNone (javaTypeIdentifier "Serializable") []

interfaceTypes :: Bool -> Aliases -> [Java.TypeParameter] -> Name -> [Java.InterfaceType]
interfaceTypes isSer aliases tparams elName = if isSer then [javaSerializableType, javaComparableType] else []
  where
    javaSerializableType = Java.InterfaceType $
      Java.ClassType [] Java.ClassTypeQualifierNone (javaTypeIdentifier "Serializable") []
    javaComparableType = Java.InterfaceType $
      Java.ClassType [] Java.ClassTypeQualifierNone (javaTypeIdentifier "Comparable") [selfTypeArg]
    selfTypeArg = Java.TypeArgumentReference $
      nameToJavaReferenceType aliases False (typeParameterToTypeArgument <$> tparams) elName Nothing

isLambdaBoundVariable :: Name -> Bool
isLambdaBoundVariable (Name v) = L.length v <= 4

-- | Check if a name looks like an unresolved type inference variable.
-- These are generated by the type inference engine and have the form 't' followed by digits
-- (e.g., t34403). Such variables should be converted to Object in Java if they're not in scope.
isUnresolvedInferenceVar :: Name -> Bool
isUnresolvedInferenceVar (Name v) = case v of
  ('t':rest) -> not (L.null rest) && all isDigit rest
  _ -> False
  where
    isDigit c = c >= '0' && c <= '9'

-- | Check if a name (possibly qualified) is lambda-bound.
-- Lambda-bound variables may be stored with qualified names (hoisted captures) or simple names.
-- References in the term may also use either form.
-- We need to match both the exact name AND the local part of qualified names.
isLambdaBoundIn :: Name -> S.Set Name -> Bool
isLambdaBoundIn name lambdaVars =
    S.member name lambdaVars
    -- For qualified names, check if any qualified lambda var has the same local name.
    -- Only match against other qualified lambda vars (hoisted captures), never against
    -- simple lambda vars, to avoid confusing module-level functions with local parameters.
    || (isQualified name && any (\lv -> isQualified lv && localNameOf lv == localNameOf name) (S.toList lambdaVars))
    -- For unqualified names, check if the local name is in lambdaVars
    || (not (isQualified name) && S.member (Name $ localNameOf name) lambdaVars)
  where
    isQualified n = Y.isJust $ qualifiedNameNamespace $ qualifyName n

-- | Find the actual lambda variable name that matches a given reference.
-- This handles the case where the parameter was declared with a qualified name
-- (e.g., "hydra/unification.Unification.unifyTypeConstraints") but the reference
-- might use a simpler name or the same qualified name.
-- Returns the name as it appears in lambdaVars, which determines the Java parameter name.
findMatchingLambdaVar :: Name -> S.Set Name -> Name
findMatchingLambdaVar name lambdaVars
    -- If the exact name is in lambdaVars, use it
    | S.member name lambdaVars = name
    -- For qualified names, find a matching qualified lambda var
    | isQualified = case L.find (\lv -> isQualifiedName lv && localNameOf lv == localNameOf name) (S.toList lambdaVars) of
        Just lv -> lv
        Nothing -> name
    -- For unqualified names, check if the local part is in lambdaVars as a simple name
    | S.member (Name $ localNameOf name) lambdaVars = Name $ localNameOf name
    | otherwise = name  -- Fallback
  where
    isQualified = Y.isJust $ qualifiedNameNamespace $ qualifyName name
    isQualifiedName n = Y.isJust $ qualifiedNameNamespace $ qualifyName n

isLocalVariable :: Name -> Bool
isLocalVariable name = Y.isNothing $ qualifiedNameNamespace $ qualifyName name

isRecursiveVariable :: Aliases -> Name -> Bool
isRecursiveVariable aliases name = S.member name (aliasesRecursiveVars aliases)

-- | Extract type arguments from a type application chain.
--   e.g. (ParseResult @ (Function T0 T1)) -> [Function T0 T1]
--        (Map @ K @ V) -> [K, V]
--        ParseResult -> []
extractTypeApplicationArgs :: Type -> [Type]
extractTypeApplicationArgs = L.reverse . go
  where
    go (TypeApplication (ApplicationType base arg)) = arg : go base
    go _ = []

javaTypeArgumentsForNamedType :: Name -> Flow Graph [Java.TypeArgument]
javaTypeArgumentsForNamedType tname = do
    params <- javaTypeParametersForType <$> requireType tname
    return $ typeParameterToTypeArgument <$> params

javaTypeArgumentsForType :: Type -> [Java.TypeArgument]
javaTypeArgumentsForType typ = L.reverse (typeParameterToTypeArgument <$> javaTypeParametersForType typ)

-- Note: this is somewhat of a hack; it compensates for the irregular way in which type parameters are currently used.
--       When this irregularity is resolved, a better approach will be to simply pick up type parameters from type applications.
javaTypeParametersForType :: Type -> [Java.TypeParameter]
javaTypeParametersForType typ = toParam <$> vars
  where
    toParam (Name v) = Java.TypeParameter [] (javaTypeIdentifier $ capitalize v) Nothing
    vars = L.nub $ boundVars typ ++ freeVars
    boundVars t = case deannotateType t of
      TypeForall (ForallType v body) -> v:(boundVars body)
      _ -> []
    freeVars = L.filter isLambdaBoundVariable $ S.toList $ freeVariablesInType typ

noComment :: Java.ClassBodyDeclaration -> Java.ClassBodyDeclarationWithComments
noComment decl = Java.ClassBodyDeclarationWithComments decl Nothing

reannotate mtyp anns term = base
  where
    base = reann anns term
    reann anns term = case anns of
      [] -> term
      (h:r) -> reann r $ TermAnnotated (AnnotatedTerm term h)

toClassDecl :: Bool -> Bool -> Aliases -> [Java.TypeParameter]
  -> Name -> Type -> Flow Graph Java.ClassDeclaration
toClassDecl isInner isSer aliases tparams elName t = case deannotateType t of
    TypeRecord rt -> declarationForRecordType isInner isSer aliases tparams elName $ rowTypeFields rt
    TypeUnion rt -> declarationForUnionType isSer aliases tparams elName $ rowTypeFields rt
    TypeForall (ForallType (Name v) body) -> toClassDecl False isSer aliases (tparams ++ [param]) elName body
      where
        param = javaTypeParameter $ capitalize v
    TypeWrap (WrappedType tname wt) -> declarationForRecordType isInner isSer aliases tparams elName
      [FieldType (Name "value") wt]
    -- Other types are not supported as class declarations, so we wrap them as record types.
    _ -> wrap t -- TODO: wrap and unwrap the corresponding terms as record terms.
  where
    wrap t' = declarationForRecordType isInner isSer aliases tparams elName [Types.field valueFieldName $ deannotateType t']

toDataDeclaration :: Aliases -> (a, TypeApplicationTerm) -> Flow Graph a
toDataDeclaration aliases (el, TypeApplicationTerm term typ) = do
  fail "not implemented" -- TODO

typeArgsOrDiamond :: [Java.TypeArgument] -> Java.TypeArgumentsOrDiamond
typeArgsOrDiamond args = if supportsDiamondOperator javaFeatures
  then Java.TypeArgumentsOrDiamondDiamond
  else Java.TypeArgumentsOrDiamondArguments args

withLambda :: JavaEnvironment -> Lambda -> (JavaEnvironment -> Flow s a) -> Flow s a
withLambda env (Lambda var mdom body) k = do
  -- Use the standard withLambdaContext to update the TypeContext
  let updateTc tc e = e { javaEnvironmentTypeContext = tc }
  withLambdaContext javaEnvironmentTypeContext updateTc env (Lambda var mdom body) $ \env1 -> do
    -- Additionally, record the lambda parameter in aliasesLambdaVars
    let aliases = javaEnvironmentAliases env1
    let aliases' = aliases { aliasesLambdaVars = S.insert var (aliasesLambdaVars aliases) }
    let env2 = env1 { javaEnvironmentAliases = aliases' }
    k env2

withTypeLambda :: JavaEnvironment -> TypeLambda -> (JavaEnvironment -> Flow s a) -> Flow s a
withTypeLambda = withTypeLambdaContext javaEnvironmentTypeContext (\tc e -> e { javaEnvironmentTypeContext = tc })
