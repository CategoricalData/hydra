
{-
TODO:
* Double-check the use of checkTypeVariables for consistency across rules
* In nominal rules (record/union/wrap intro and elim), double-check that we are checking provided data against the schema
* Use the inference debug flag to enable / disable code which is purely for checking and not for reconstruction.
* When the debug flag is set, call checkType from hydra.inference rules
-}

module Hydra.Sources.Kernel.Terms.Checking where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  allEqual, applyTypeArgumentsToType, checkForUnboundTypeVariables, checkNominalApplication,
  checkSameType, checkType, checkTypeSubst, checkTypeVariables, containsInScopeTypeVars, normalizeTypeFreeVars, toFContext,
  typeListsEffectivelyEqual, typeOf, typeOfAnnotatedTerm, typeOfApplication, typeOfCaseStatement,
  typeOfEither, typeOfFunction, typeOfInjection, typeOfLambda, typeOfLet, typeOfLiteral, typeOfList,
  typeOfMap, typeOfMaybe, typeOfPair, typeOfPrimitive, typeOfProjection, typeOfRecord, typeOfSet,
  typeOfTypeApplication, typeOfTypeLambda, typeOfUnion, typeOfUnit, typeOfUnwrap, typeOfVariable,
  typeOfWrap, typeOfWrappedTerm, typesAllEffectivelyEqual, typesEffectivelyEqual)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Constants    as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Formatting   as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Reflect      as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas      as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core    as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Meta    as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution


ns :: Namespace
ns = Namespace "hydra.checking"

module_ :: Module
module_ = Module ns elements
    [Constants.ns, ExtractCore.ns, Formatting.ns, Lexical.ns, Reflect.ns, Rewriting.ns,
      Schemas.ns, ShowCore.ns, ShowMeta.ns, Substitution.ns]
    kernelTypesNamespaces $
    Just "Type checking and type reconstruction (type-of) for the results of Hydra unification and inference"
  where
    elements = [
      toBinding allEqual,
      toBinding applyTypeArgumentsToType,
      toBinding checkForUnboundTypeVariables,
      toBinding checkNominalApplication,
      toBinding checkSameType,
      toBinding checkType,
      toBinding checkTypeSubst,
      toBinding checkTypeVariables,
      toBinding toFContext,
      toBinding typeListsEffectivelyEqual,
      toBinding typeOf,
      toBinding typeOfAnnotatedTerm,
      toBinding typeOfApplication,
      toBinding typeOfCaseStatement,
      toBinding typeOfEither,
      toBinding typeOfInjection,
      toBinding typeOfLambda,
      toBinding typeOfLet,
      toBinding typeOfList,
      toBinding typeOfLiteral,
      toBinding typeOfMap,
      toBinding typeOfMaybe,
      toBinding typeOfPair,
      toBinding typeOfPrimitive,
      toBinding typeOfProjection,
      toBinding typeOfRecord,
      toBinding typeOfSet,
      toBinding typeOfTypeApplication,
      toBinding typeOfTypeLambda,
      toBinding typeOfUnit,
      toBinding typeOfUnwrap,
      toBinding typeOfVariable,
      toBinding typeOfWrappedTerm,
      toBinding containsInScopeTypeVars,
      toBinding normalizeTypeFreeVars,
      toBinding typesAllEffectivelyEqual,
      toBinding typesEffectivelyEqual]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

noTypeArgs :: TTerm [Type]
noTypeArgs = list ([] :: [TTerm Type])

--

-- TODO: move this
allEqual :: TBinding ([a] -> Bool)
allEqual = define "allEqual" $
  "els" ~>
  Logic.ifElse (Lists.null $ var "els")
    true
    (Lists.foldl
       ("b" ~> "t" ~> Logic.and (var "b") (Equality.equal (var "t") (Lists.head (var "els"))))
       true
       (Lists.tail $ var "els"))

applyTypeArgumentsToType :: TBinding (TypeContext -> [Type] -> Type -> Flow s Type)
applyTypeArgumentsToType = define "applyTypeArgumentsToType" $
  doc "Apply type arguments to a type, substituting forall-bound variables" $
  "tx" ~> "typeArgs" ~> "t" ~>
  "nonnull" <~ (cases _Type (var "t")
    (Just $ Flows.fail $ Strings.cat $ list [
      string "not a forall type: ",
      ShowCore.type_ @@ var "t",
      string ". Trying to apply ",
      Literals.showInt32 (Lists.length $ var "typeArgs"),
      string " type args: ",
      Formatting.showList @@ ShowCore.type_ @@ var "typeArgs",
      string ". Context has vars: {",
      Strings.intercalate (string ", ") (Lists.map (unaryFunction $ Core.unName) $ Maps.keys $ Typing.typeContextTypes $ var "tx"),
      string "}"]) [
    _Type_forall>>: "ft" ~>
      "v" <~ Core.forallTypeParameter (var "ft") $
      "tbody" <~ Core.forallTypeBody (var "ft") $
      applyTypeArgumentsToType
        @@ var "tx"
        @@ (Lists.tail $ var "typeArgs")
        @@ (Substitution.substInType
          @@ (Typing.typeSubst $ Maps.singleton (var "v") (Lists.head $ var "typeArgs"))
          @@ (var "tbody"))]) $
  -- Only check type variables when there are no more type arguments to apply.
  -- When there ARE type arguments, the type may contain forall-bound variables that will
  -- be substituted by the type arguments. Those variables aren't in scope yet but will be resolved.
  Logic.ifElse (Lists.null $ var "typeArgs")
    (exec (checkTypeVariables @@ var "tx" @@ var "t") $ produce $ var "t")
    (var "nonnull")

checkForUnboundTypeVariables :: TBinding (InferenceContext -> Term -> Flow s ())
checkForUnboundTypeVariables = define "checkForUnboundTypeVariables" $
  doc "Check that a term has no unbound type variables" $
  "cx" ~> "term0" ~>
  "svars" <~ Sets.fromList (Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "checkRecursive" <~ ("vars" ~> "trace" ~> "lbinding" ~> "term" ~>
    "recurse" <~ var "checkRecursive" @@ var "vars" @@ var "trace" @@ var "lbinding" $
    "dflt" <~ (
      exec (Flows.mapList (var "recurse") (Rewriting.subterms @@ var "term")) $
      produce unit) $
    "check" <~ ("typ" ~>
      "freevars" <~ Rewriting.freeVariablesInType @@ var "typ" $
      "badvars" <~ Sets.difference (Sets.difference (var "freevars") (var "vars")) (var "svars") $
      Logic.ifElse (Sets.null $ var "badvars")
        (produce unit)
        (Flows.fail $ (string "unbound type variables: {")
          ++ (Strings.intercalate (string ", ") (Lists.map (unaryFunction Core.unName) $ Sets.toList $ var "badvars"))
          ++ (string "} in type ") ++ (ShowCore.type_ @@ var "typ") ++ (string " at path: ") ++ (Strings.intercalate (string " >> ") (Lists.reverse $ var "trace"))
          ++ (optCases (var "lbinding")
            (string "none")
            ("binding" ~>
                 (string ". bound term = ") ++ (ShowCore.term @@ (Core.bindingTerm $ var "binding"))
              ++ (string ". bound type = ") ++ (optCases (Core.bindingType $ var "binding") (string "none") (ShowCore.typeScheme)))))) $
    "checkOptional" <~ ("m" ~>
      exec (Flows.mapMaybe (var "check") (var "m")) $
      produce unit) $
    "checkOptionalList" <~ ("ml" ~>
      exec (Flows.mapMaybe ("l" ~> Flows.mapList (var "check") (var "l")) (var "ml")) $
      produce unit) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> var "dflt",
        _Function_lambda>>: "l" ~>
          exec (var "checkOptional" @@ (Core.lambdaDomain $ var "l")) $
          var "recurse" @@ (Core.lambdaBody $ var "l")],
      _Term_let>>: "l" ~>
        "forBinding" <~ ("b" ~>
          "bterm" <~ Core.bindingTerm (var "b") $
          "newVars" <~ optCases (Core.bindingType $ var "b")
             (var "vars")
             ("ts" ~> Sets.union (var "vars") (Sets.fromList $ Core.typeSchemeVariables $ var "ts")) $
          "newTrace" <~ Lists.cons (Core.unName $ Core.bindingName $ var "b") (var "trace") $
          var "checkRecursive" @@ var "newVars" @@ var "newTrace" @@ (just $ var "b") @@ var "bterm") $
        exec (Flows.mapList (var "forBinding") $ Core.letBindings $ var "l") $
        var "recurse" @@ (Core.letBody $ var "l"),
      _Term_typeApplication>>: "tt" ~>
        exec (var "check" @@ (Core.typeApplicationTermType $ var "tt")) $
        var "recurse" @@ (Core.typeApplicationTermBody $ var "tt"),
      _Term_typeLambda>>: "tl" ~>
        exec (var "check" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl")) $
        var "recurse" @@ (Core.typeLambdaBody $ var "tl")]) $
  var "checkRecursive" @@ Sets.empty @@ list [string "top level"] @@ nothing @@ var "term0"

checkNominalApplication ::  TBinding (TypeContext -> Name -> [Type] -> Flow s ())
checkNominalApplication = define "checkNominalApplication" $
  doc "Check that a nominal type is applied to the correct number of type arguments" $
  "tx" ~> "tname" ~> "typeArgs" ~>
  "schemaType" <<~ Schemas.requireSchemaType @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "vars" <~ Core.typeSchemeVariables (var "schemaType") $
  "body" <~ Core.typeSchemeType (var "schemaType") $
  "varslen" <~ Lists.length (var "vars") $
  "argslen" <~ Lists.length (var "typeArgs") $
  Logic.ifElse (Equality.equal (var "varslen") (var "argslen"))
    (produce unit)
    (Flows.fail $ (string "nominal type ") ++ Core.unName (var "tname") ++ (string " applied to the wrong number of type arguments: ")
      ++ (string "(expected ") ++ Literals.showInt32 (var "varslen") ++ (string " arguments, got ")
      ++ Literals.showInt32 (var "argslen") ++ (string "): ")
      ++ (Formatting.showList @@ (ShowCore.type_) @@ (var "typeArgs")))

checkSameType :: TBinding (TypeContext -> String -> [Type] -> Flow s Type)
checkSameType = define "checkSameType" $
  doc "Ensure all types in a list are equal and return the common type" $
  "tx" ~> "desc" ~> "types" ~>
  Logic.ifElse (typesAllEffectivelyEqual @@ var "tx" @@ var "types")
    (Flows.pure $ Lists.head $ var "types")
    (Flows.fail $ Strings.cat $ list [
      string "unequal types ",
      (Formatting.showList @@ ShowCore.type_ @@ var "types"),
      string " in ",
      var "desc"])

-- TODO: unused
checkType :: TBinding (TypeContext -> Term -> Type -> Flow s ())
checkType = define "checkType" $
  doc "Check that a term has the expected type" $
  "tx" ~> "term" ~> "typ" ~>
  "cx" <~ Typing.typeContextInferenceContext (var "tx") $
  "vars" <~ Typing.typeContextTypeVariables (var "tx") $
  Logic.ifElse (Constants.debugInference)
    ("t0" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "term" $
      Logic.ifElse (typesEffectivelyEqual @@ var "tx" @@ var "t0" @@ var "typ")
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "type checking failed: expected ",
          ShowCore.type_ @@ var "typ",
          string " but found ",
          ShowCore.type_ @@ var "t0"]))
    (Flows.pure unit)

checkTypeSubst :: TBinding (InferenceContext -> TypeSubst -> Flow s TypeSubst)
checkTypeSubst = define "checkTypeSubst" $
  doc ("Sanity-check a type substitution arising from unification. Specifically, check that schema types have not been"
    <> " inappropriately unified with type variables inferred from terms.") $
  "cx" ~> "subst" ~>
  "s" <~ Typing.unTypeSubst (var "subst") $
  "vars" <~ Sets.fromList (Maps.keys $ var "s") $
  "suspectVars" <~ Sets.intersection (var "vars") (Sets.fromList $ Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "isNominal" <~ ("ts" ~> cases _Type (Rewriting.deannotateType @@ (Core.typeSchemeType $ var "ts"))
    (Just false) [
    _Type_record>>: constant true,
    _Type_union>>: constant true,
    _Type_wrap>>: constant true]) $
  "badVars" <~ Sets.fromList (Lists.filter
    ("v" ~> Maybes.maybe false (var "isNominal") $
      Lexical.dereferenceSchemaType @@ var "v" @@ (Typing.inferenceContextSchemaTypes $ var "cx"))
    (Sets.toList $ var "suspectVars")) $
  "badPairs" <~ Lists.filter ("p" ~> Sets.member (Pairs.first $ var "p") (var "badVars")) (Maps.toList $ var "s") $
  "printPair" <~ ("p" ~> (Core.unName $ Pairs.first $ var "p") ++ (string " --> ") ++ (ShowCore.type_ @@ Pairs.second (var "p"))) $
  Logic.ifElse (Sets.null $ var "badVars")
    (produce $ var "subst")
    (Flows.fail $ (string "Schema type(s) incorrectly unified: {")
      ++ (Strings.intercalate (string ", ") (Lists.map (var "printPair") (var "badPairs")))
      ++ (string "}"))

checkTypeVariables :: TBinding (TypeContext -> Type -> Flow s ())
checkTypeVariables = define "checkTypeVariables" $
  doc "Check that all type variables in a type are bound. NOTE: This check is currently disabled to allow phantom type variables from polymorphic instantiation to pass through. The proper fix is to ensure `typeOf` doesn't create fresh variables for post-inference code." $
  "_tx" ~> "_typ" ~>
  -- Disabled: phantom type variables from polymorphic instantiation cause false positives.
  -- The inference pass has already validated type variables via checkForUnboundTypeVariables.
  Flows.pure unit

checkTypeVariablesDisabled :: TBinding (TypeContext -> Type -> Flow s ())
checkTypeVariablesDisabled = define "checkTypeVariablesDisabled" $
  doc "Original checkTypeVariables implementation (disabled)" $
  "tx" ~> "typ" ~>
  "cx" <~ Typing.typeContextInferenceContext (var "tx") $
  "vars" <~ Typing.typeContextTypeVariables (var "tx") $
  "dflt" <~ (
    exec (Flows.mapList (var "checkTypeVariablesDisabled" @@ var "tx") (Rewriting.subtypes @@ var "typ")) $
    produce unit) $
  "check" <~ (cases _Type (var "typ")
    (Just $ var "dflt") [
    _Type_forall>>: "ft" ~> var "checkTypeVariablesDisabled"
      @@ (Typing.typeContextWithTypeVariables (var "tx") (Sets.insert (Core.forallTypeParameter $ var "ft") (var "vars")))
      @@ (Core.forallTypeBody $ var "ft"),
    _Type_variable>>: "v" ~> Logic.ifElse (Sets.member (var "v") (var "vars"))
      (Flows.pure unit)
      (Logic.ifElse (Maps.member (var "v") (Typing.inferenceContextSchemaTypes (var "cx")))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "unbound type variable \"",
          Core.unName $ var "v",
          string "\" in ",
          ShowCore.type_ @@ var "typ",
          string ". Local variables: {",
          Strings.intercalate (string ", ") (Lists.map (unaryFunction $ Core.unName) $ Sets.toList $ var "vars"),
          string "}, schema variables: {",
          Strings.intercalate (string ", ") (Lists.map (unaryFunction $ Core.unName) $ Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx"),
          string "}"]))]) $
  trace (Strings.cat $ list [
    string "checking variables of: ",
    ShowCore.type_ @@ var "typ"]) $
  var "check"

-- TODO: unused (?). This function can be used to construct Typing.typeContextTypes from Typing.typeContextInferenceContext
toFContext :: TBinding (InferenceContext -> M.Map Name Type)
toFContext = define "toFContext" $
  doc "Convert an inference context to a type environment by converting type schemes to System F types" $
  "cx" ~> Maps.map (Schemas.typeSchemeToFType) $ Typing.inferenceContextDataTypes $ var "cx"

typeListsEffectivelyEqual :: TBinding (TypeContext -> [Type] -> [Type] -> Bool)
typeListsEffectivelyEqual = define "typeListsEffectivelyEqual" $
  doc "Check whether two lists of types are effectively equal, disregarding type aliases" $
  "tx" ~> "tlist1" ~> "tlist2" ~>
  Logic.ifElse (Equality.equal (Lists.length (var "tlist1")) (Lists.length (var "tlist2")))
    (Lists.foldl (binaryFunction Logic.and) true $
      Lists.zipWith (typesEffectivelyEqual @@ var "tx") (var "tlist1") (var "tlist2"))
    false

typeOf :: TBinding (TypeContext -> [Type] -> Term -> Flow s Type)
typeOf = define "typeOf" $
  doc "Given a type context, reconstruct the type of a System F term" $
  "tx" ~> "typeArgs" ~> "term" ~>
  "check" <~ (cases _Term (var "term")
    (Just $ Flows.fail $ Strings.cat $ list [
      string "unsupported term variant in typeOf: ",
      ShowMeta.termVariant @@ (Reflect.termVariant @@ var "term")]) [
    _Term_annotated>>: typeOfAnnotatedTerm @@ var "tx" @@ var "typeArgs",
    _Term_application>>: typeOfApplication @@ var "tx" @@ var "typeArgs",
    _Term_either>>: typeOfEither @@ var "tx" @@ var "typeArgs",
    _Term_function>>: "f" ~>
      cases _Function (var "f") Nothing [
        _Function_elimination>>: "elm" ~>
          cases _Elimination (var "elm") Nothing [
            _Elimination_record>>: typeOfProjection @@ var "tx" @@ var "typeArgs",
            _Elimination_union>>: typeOfCaseStatement @@ var "tx" @@ var "typeArgs",
            _Elimination_wrap>>: typeOfUnwrap @@ var "tx" @@ var "typeArgs"],
        _Function_lambda>>: typeOfLambda @@ var "tx" @@ var "typeArgs",
        _Function_primitive>>: typeOfPrimitive @@ var "tx" @@ var "typeArgs"],
    _Term_let>>: typeOfLet @@ var "tx" @@ var "typeArgs",
    _Term_list>>: typeOfList @@ var "tx" @@ var "typeArgs",
    _Term_literal>>: typeOfLiteral @@ var "tx" @@ var "typeArgs",
    _Term_map>>: typeOfMap @@ var "tx" @@ var "typeArgs",
    _Term_maybe>>: typeOfMaybe @@ var "tx" @@ var "typeArgs",
    _Term_pair>>: typeOfPair @@ var "tx" @@ var "typeArgs",
    _Term_record>>: typeOfRecord @@ var "tx" @@ var "typeArgs",
    _Term_set>>: typeOfSet @@ var "tx" @@ var "typeArgs",
    _Term_typeApplication>>: typeOfTypeApplication @@ var "tx" @@ var "typeArgs",
    _Term_typeLambda>>: typeOfTypeLambda @@ var "tx" @@ var "typeArgs",
    _Term_union>>: typeOfInjection @@ var "tx" @@ var "typeArgs",
    _Term_unit>>: constant $ typeOfUnit @@ var "tx" @@ var "typeArgs",
    _Term_variable>>: typeOfVariable @@ var "tx" @@ var "typeArgs",
    _Term_wrap>>: typeOfWrappedTerm @@ var "tx" @@ var "typeArgs"]) $
  trace (string "typeOf") $
--  trace (string "typeOf " ++ (ShowCore.term @@ var "term")) $ -- For debugging only; risk of slow execution and stack overflow on deeply nested terms
  var "check"

typeOfAnnotatedTerm :: TBinding (TypeContext -> [Type] -> AnnotatedTerm -> Flow s Type)
typeOfAnnotatedTerm = define "typeOfAnnotatedTerm" $
  doc "Reconstruct the type of an annotated term" $
  "tx" ~> "typeArgs" ~> "at" ~>
  typeOf @@ var "tx" @@ var "typeArgs" @@ Core.annotatedTermBody (var "at")

typeOfApplication :: TBinding (TypeContext -> [Type] -> Application -> Flow s Type)
typeOfApplication = define "typeOfApplication" $
  doc "Reconstruct the type of an application term" $
  "tx" ~> "typeArgs" ~> "app" ~>
  "fun" <~ Core.applicationFunction (var "app") $
  "arg" <~ Core.applicationArgument (var "app") $
  "tryType" <~ ("tfun" ~> "targ" ~> cases _Type (var "tfun")
    (Just $ Flows.fail $ Strings.cat $ list [
      string "left hand side of application (",
      ShowCore.term @@ var "fun",
      string ") is not function-typed (",
      ShowCore.type_ @@ var "tfun",
      string ")",
      string ". types: ", Strings.intercalate (string ", ") (Lists.map
        ("p" ~> Strings.cat $ list [Core.unName (Pairs.first $ var "p"), string ": ", ShowCore.type_ @@ (Pairs.second $ var "p")]) $
        Maps.toList $ Typing.typeContextTypes $ var "tx")]) [
    -- These forall types can arise from bindUnboundTypeVariables
    _Type_forall>>: "ft" ~> var "tryType" @@ (Core.forallTypeBody (var "ft")) @@ var "targ",
    _Type_function>>: "ft" ~>
      "dom" <~ Core.functionTypeDomain (var "ft") $
      "cod" <~ Core.functionTypeCodomain (var "ft") $
      Logic.ifElse (typesEffectivelyEqual @@ var "tx" @@ var "dom" @@ var "targ")
        (Flows.pure $ var "cod")
        (Flows.fail $ Strings.cat $ list [
          string "in application, expected ",
          ShowCore.type_ @@ var "dom",
          string " but found ",
          ShowCore.type_ @@ var "targ"]),
    -- Handle type variables: if the function type is a type variable, treat it as polymorphic
    -- and return a fresh type variable as the result type. This can happen when inference
    -- produces type variables that aren't fully resolved.
    _Type_variable>>: "v" ~>
      Flows.map (unaryFunction Core.typeVariable) (Schemas.freshName)]) $
  -- Note: We don't call checkTypeVariables on tfun/targ because they may contain
  -- phantom type variables from polymorphic instantiation that aren't in scope.
  -- The type matching in tryType ensures type correctness.
  "tfun" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "fun" $
  "targ" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "arg" $
  "t" <<~ var "tryType" @@ var "tfun" @@ var "targ" $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfCaseStatement :: TBinding (TypeContext -> [Type] -> CaseStatement -> Flow s Type)
typeOfCaseStatement = define "typeOfCaseStatement" $
  doc "Reconstruct the type of a case statement" $
  "tx" ~> "typeArgs" ~> "cs" ~>
  "tname" <~ Core.caseStatementTypeName (var "cs") $

  "dflt" <~ Core.caseStatementDefault (var "cs") $
  "cases" <~ Core.caseStatementCases (var "cs") $
  "cterms" <~ Lists.map (unaryFunction Core.fieldTerm) (var "cases") $
  "tdflt" <<~ Flows.mapMaybe ("e" ~> typeOf @@ var "tx" @@ noTypeArgs  @@ var "e") (var "dflt") $
  "tcterms" <<~ Flows.mapList ("e" ~> typeOf @@ var "tx" @@ noTypeArgs  @@ var "e") (var "cterms") $
  "fcods" <<~ Flows.mapList ("t" ~> Flows.map (unaryFunction Core.functionTypeCodomain) $ ExtractCore.functionType @@ var "t") (var "tcterms") $
  "cods" <~ Maybes.cat (Lists.cons (var "tdflt") $ Lists.map (unaryFunction Maybes.pure) (var "fcods")) $
  "cod" <<~ checkSameType @@ var "tx" @@ (string "case branches") @@ var "cods" $
--  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
--  "scod" <~ Substitution.substInType @@ var "subst" @@ var "cod" $

  produce $ Core.typeFunction $ Core.functionType
    (Schemas.nominalApplication @@ var "tname"  @@ var "typeArgs")
    (var "cod")

typeOfEither :: TBinding (TypeContext -> [Type] -> Prelude.Either Term Term -> Flow s Type)
typeOfEither = define "typeOfEither" $
  doc "Reconstruct the type of an either value" $
  "tx" ~> "typeArgs" ~> "et" ~>
  "checkLength" <~ (
    "n" <~ Lists.length (var "typeArgs") $
    Logic.ifElse (Equality.equal (var "n") (int32 2))
      (Flows.pure unit)
      (Flows.fail $ (string "either type requires 2 type arguments, got ") ++ Literals.showInt32 (var "n"))) $
  exec (var "checkLength") $
  Eithers.either_
    ("leftTerm" ~>
      "leftType" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "leftTerm" $
      exec (checkTypeVariables @@ var "tx" @@ var "leftType") $
      Flows.pure $ Core.typeEither $ Core.eitherType (var "leftType") (Lists.at (int32 1) $ var "typeArgs"))
    ("rightTerm" ~>
      "rightType" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "rightTerm" $
      exec (checkTypeVariables @@ var "tx" @@ var "rightType") $
      Flows.pure $ Core.typeEither $ Core.eitherType (Lists.at (int32 0) $ var "typeArgs") (var "rightType"))
    (var "et")

typeOfInjection :: TBinding (TypeContext -> [Type] -> Injection -> Flow s Type)
typeOfInjection = define "typeOfInjection" $
  doc "Reconstruct the type of a union injection" $
  "tx" ~> "typeArgs" ~> "injection" ~>
  "tname" <~ Core.injectionTypeName (var "injection") $

  -- The following is only for checking, not for reconstruction
  "field" <~ Core.injectionField (var "injection") $
  "fname" <~ Core.fieldName (var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  "schemaType" <<~ Schemas.requireSchemaType @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.unionType @@ var "tname" @@ var "sbody" $
  "ftyp" <<~ Schemas.findFieldType @@ var "fname" @@ var "sfields" $

  produce $ Schemas.nominalApplication @@ var "tname"  @@ var "typeArgs"

typeOfLambda :: TBinding (TypeContext -> [Type] -> Lambda -> Flow s Type)
typeOfLambda = define "typeOfLambda" $
  doc "Reconstruct the type of a lambda function" $
  "tx" ~> "typeArgs" ~> "l" ~>
  "v" <~ Core.lambdaParameter (var "l") $
  "mdom" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "tbody" <<~ optCases (var "mdom")
    (Flows.fail (string "untyped lambda"))
    ("dom" ~>
      exec (checkTypeVariables @@ var "tx" @@ var "dom") $
      "types2" <~ Maps.insert (var "v") (var "dom") (Typing.typeContextTypes $ var "tx") $
      "cod" <<~ typeOf @@ (Typing.typeContextWithTypes (var "tx") $ var "types2") @@ noTypeArgs  @@ var "body" $
      exec (checkTypeVariables @@ var "tx" @@ var "cod") $
      Flows.pure $ Core.typeFunction $ Core.functionType (var "dom") (var "cod")) $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "tbody"

typeOfLet :: TBinding (TypeContext -> [Type] -> Let -> Flow s Type)
typeOfLet = define "typeOfLet" $
  doc "Reconstruct the type of a let binding" $
  "tx" ~> "typeArgs" ~> "letTerm" ~>
  "bs" <~ Core.letBindings (var "letTerm") $
  "body" <~ Core.letBody (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bs") $
  "bterms" <~ Lists.map (unaryFunction Core.bindingTerm) (var "bs") $
  "bindingType" <~ ("b" ~>
    Maybes.maybe
      (Flows.fail $ Strings.cat $ list [
        string "untyped let binding: ",
        ShowCore.binding @@ var "b"])
      ("ts" ~> Flows.pure $ Schemas.typeSchemeToFType @@ var "ts")
      (Core.bindingType $ var "b")) $
  -- Previously inferred types for each binding
  "btypes" <<~ Flows.mapList (var "bindingType") (var "bs") $
  -- Extended type context (note: new bindings override old ones)
  "tx2" <~ (Typing.typeContextWithTypes (var "tx")
    (Maps.union
      (Maps.fromList $ Lists.zip (var "bnames") (var "btypes"))
      (Typing.typeContextTypes $ var "tx"))) $
  -- Reconstructed type for each binding
  "typeofs" <<~ Flows.mapList (typeOf @@ var "tx2" @@ noTypeArgs ) (var "bterms") $
  -- Note: We don't call checkTypeVariables on btypes because they come from type schemes that
  -- may contain phantom type variables from polymorphic instantiation. These were already
  -- validated during inference. We also don't check typeofs since they also come from inference.
  -- The type equality check below ensures they match.
  "t" <<~ Logic.ifElse (typeListsEffectivelyEqual @@ var "tx" @@ var "typeofs" @@ var "btypes")
    (typeOf @@ var "tx2" @@ noTypeArgs  @@ var "body")
    (Flows.fail $ Strings.cat $ list [
      string "binding types disagree: ",
      Formatting.showList @@ ShowCore.type_ @@ var "btypes",
      string " and ",
      Formatting.showList @@ ShowCore.type_ @@ var "typeofs",
      string " from terms: ",
      Formatting.showList @@ ShowCore.term @@ var "bterms"]) $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfList :: TBinding (TypeContext -> [Type] -> [Term] -> Flow s Type)
typeOfList = define "typeOfList" $
  doc "Reconstruct the type of a list" $
  "tx" ~> "typeArgs" ~> "els" ~>
  Logic.ifElse (Lists.null $ var "els")
    -- Empty list is polymorphic
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 1))
      (Flows.pure $ Core.typeList $ Lists.head $ var "typeArgs")
      (Flows.fail (string "list type applied to more or less than one argument")))
    -- Nonempty list must have elements of the same type
    ( "eltypes" <<~ Flows.mapList
       (typeOf @@ var "tx" @@ noTypeArgs )
       (var "els") $
       "unifiedType" <<~ checkSameType @@ var "tx" @@ (string "list elements") @@ var "eltypes" $
       -- Verify the unified type is well-formed in the current scope
       exec (checkTypeVariables @@ var "tx" @@ var "unifiedType") $
       Flows.pure $ Core.typeList $ var "unifiedType")

typeOfLiteral :: TBinding (TypeContext -> [Type] -> Literal -> Flow s Type)
typeOfLiteral = define "typeOfLiteral" $
  doc "Reconstruct the type of a literal" $
  "tx" ~> "typeArgs" ~> "lit" ~>
  "t" <~ Core.typeLiteral (Reflect.literalType @@ var "lit") $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfMap :: TBinding (TypeContext -> [Type] -> M.Map Term Term -> Flow s Type)
typeOfMap = define "typeOfMap" $
  doc "Reconstruct the type of a map" $
  "tx" ~> "typeArgs" ~> "m" ~>
  "nonnull" <~ (
    "pairs" <~ Maps.toList (var "m") $
    "kt" <<~ Flows.bind
      (Flows.mapList (typeOf @@ var "tx" @@ noTypeArgs ) $
        Lists.map (unaryFunction Pairs.first) (var "pairs"))
      (checkSameType @@ var "tx" @@ (string "map keys")) $
    "vt" <<~ Flows.bind
      (Flows.mapList (typeOf @@ var "tx" @@ noTypeArgs ) $
        Lists.map (unaryFunction Pairs.second) (var "pairs"))
      (checkSameType @@ var "tx" @@ (string "map values")) $
    exec (checkTypeVariables @@ var "tx" @@ var "kt") $
    exec (checkTypeVariables @@ var "tx" @@ var "vt") $
    applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs"
      @@ (Core.typeMap $ Core.mapType (var "kt") (var "vt"))) $
  Logic.ifElse (Maps.null $ var "m")
    -- Empty map is polymorphic
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 2))
      (Flows.pure $ Core.typeMap $ Core.mapType
        (Lists.at (int32 0) $ var "typeArgs")
        (Lists.at (int32 1) $ var "typeArgs"))
      (Flows.fail (string "map type applied to more or less than two arguments")))
    -- Nonempty map must have keys and values of the same type
    (var "nonnull")

typeOfMaybe :: TBinding (TypeContext -> [Type] -> Maybe Term -> Flow s Type)
typeOfMaybe = define "typeOfMaybe" $
  doc "Reconstruct the type of an optional value" $
  "tx" ~> "typeArgs" ~> "mt" ~>
    -- Nothing case is polymorphic
  "forNothing" <~ (
    "n" <~ Lists.length (var "typeArgs") $
    Logic.ifElse (Equality.equal (var "n") (int32 1))
      (Flows.pure $ Core.typeMaybe $ Lists.head $ var "typeArgs")
      (Flows.fail $ (string "optional type applied to ") ++ Literals.showInt32 (var "n") ++ (string " argument(s). Expected 1."))) $
    -- Just case: infer type of the contained term
  "forJust" <~ ("term" ~>
    "t" <<~ (
      "termType" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "term" $
       exec (checkTypeVariables @@ var "tx" @@ var "termType") $
       Flows.pure $ Core.typeMaybe $ var "termType") $
    applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "t") $
  optCases (var "mt") (var "forNothing") (var "forJust")

typeOfPair :: TBinding (TypeContext -> [Type] -> (Term, Term) -> Flow s Type)
typeOfPair = define "typeOfPair" $
  doc "Reconstruct the type of a pair" $
  "tx" ~> "typeArgs" ~> "p" ~>
  "checkLength" <~ (
    "n" <~ Lists.length (var "typeArgs") $
    Logic.ifElse (Equality.equal (var "n") (int32 2))
      (Flows.pure unit)
      (Flows.fail $ (string "pair type requires 2 type arguments, got ") ++ Literals.showInt32 (var "n"))) $
  exec (var "checkLength") $
  "pairFst" <~ Pairs.first (var "p") $
  "pairSnd" <~ Pairs.second (var "p") $
  "firstType" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "pairFst" $
  exec (checkTypeVariables @@ var "tx" @@ var "firstType") $
  "secondType" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "pairSnd" $
  exec (checkTypeVariables @@ var "tx" @@ var "secondType") $
  Flows.pure $ Core.typePair $ Core.pairType (var "firstType") (var "secondType")

typeOfPrimitive :: TBinding (TypeContext -> [Type] -> Name -> Flow s Type)
typeOfPrimitive = define "typeOfPrimitive" $
  doc "Reconstruct the type of a primitive function" $
  "tx" ~> "typeArgs" ~> "name" ~>
  "ts" <<~ optCases
    (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ Typing.typeContextInferenceContext $ var "tx"))
    (Flows.fail $ Strings.cat $ list [
      string "no such primitive: ",
      Core.unName $ var "name"])
    (Schemas.instantiateTypeScheme) $
  "t" <~ Schemas.typeSchemeToFType @@ var "ts" $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfProjection :: TBinding (TypeContext -> [Type] -> Projection -> Flow s Type)
typeOfProjection = define "typeOfProjection" $
  doc "Reconstruct the type of a record projection" $
  "tx" ~> "typeArgs" ~> "p" ~>
  "tname" <~ Core.projectionTypeName (var "p") $
  "fname" <~ Core.projectionField (var "p") $
  "schemaType" <<~ Schemas.requireSchemaType @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ExtractCore.recordType @@ var "tname" @@ var "sbody" $
  "ftyp" <<~ Schemas.findFieldType @@ var "fname" @@ var "sfields" $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
  "sftyp" <~ Substitution.substInType @@ var "subst" @@ var "ftyp" $
  produce $ Core.typeFunction $ Core.functionType
    (Schemas.nominalApplication @@ var "tname" @@ var "typeArgs")
    (var "sftyp")

typeOfRecord :: TBinding (TypeContext -> [Type] -> Record -> Flow s Type)
typeOfRecord = define "typeOfRecord" $
  doc "Reconstruct the type of a record" $
  "tx" ~> "typeArgs" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $

  -- The following is only for checking, not for reconstruction
  "fields" <~ Core.recordFields (var "record") $
  "ftypes" <<~ Flows.mapList
    (typeOf @@ var "tx" @@ noTypeArgs )
    (Lists.map (unaryFunction Core.fieldTerm) (var "fields")) $
  exec (Flows.mapList (checkTypeVariables @@ var "tx") (var "ftypes")) $

  produce $ Schemas.nominalApplication @@ var "tname" @@ var "typeArgs"

typeOfSet :: TBinding (TypeContext -> [Type] -> S.Set Term -> Flow s Type)
typeOfSet = define "typeOfSet" $
  doc "Reconstruct the type of a set" $
  "tx" ~> "typeArgs" ~> "els" ~>
  Logic.ifElse (Sets.null $ var "els")
    -- Empty set is polymorphic
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 1))
      (Flows.pure $ Core.typeSet $ Lists.head $ var "typeArgs")
      (Flows.fail (string "set type applied to more or less than one argument")))
    -- Nonempty set must have elements of the same type
    ( "eltypes" <<~ Flows.mapList
       (typeOf @@ var "tx" @@ noTypeArgs )
       (Sets.toList $ var "els") $
       "unifiedType" <<~ checkSameType @@ var "tx" @@ (string "set elements") @@ var "eltypes" $
       -- Verify the unified type is well-formed in the current scope
       exec (checkTypeVariables @@ var "tx" @@ var "unifiedType") $
       produce $ Core.typeSet $ var "unifiedType")

typeOfTypeApplication :: TBinding (TypeContext -> [Type] -> TypeApplicationTerm -> Flow s Type)
typeOfTypeApplication = define "typeOfTypeApplication" $
  doc "Reconstruct the type of a type application term" $
  "tx" ~> "typeArgs" ~> "tyapp" ~>
  "body" <~ Core.typeApplicationTermBody (var "tyapp") $
  "t" <~ Core.typeApplicationTermType (var "tyapp") $
  typeOf @@ var "tx" @@ Lists.cons (var "t") (var "typeArgs") @@ var "body"

typeOfTypeLambda :: TBinding (TypeContext -> [Type] -> TypeLambda -> Flow s Type)
typeOfTypeLambda = define "typeOfTypeLambda" $
  doc "Reconstruct the type of a type lambda (type abstraction) term" $
  "tx" ~> "typeArgs" ~> "tl" ~>
  "v" <~ Core.typeLambdaParameter (var "tl") $
  "body" <~ Core.typeLambdaBody (var "tl") $
  "vars" <~ Typing.typeContextTypeVariables (var "tx") $
  "tx2" <~ Typing.typeContextWithTypeVariables (var "tx") (Sets.insert (var "v") (var "vars")) $
  "t1" <<~ typeOf @@ var "tx2" @@ noTypeArgs  @@ var "body" $
  exec (checkTypeVariables @@ var "tx2" @@ var "t1") $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs"
    @@ (Core.typeForall $ Core.forallType (var "v") (var "t1"))

typeOfUnit :: TBinding (TypeContext -> [Type] -> Flow s Type)
typeOfUnit = define "typeOfUnit" $
  doc "Reconstruct the type of the unit term" $
  "tx" ~> "typeArgs" ~>
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ Core.typeUnit

typeOfUnwrap :: TBinding (TypeContext -> [Type] -> Name -> Flow s Type)
typeOfUnwrap = define "typeOfUnwrap" $
  doc "Reconstruct the type of an unwrap operation" $
  "tx" ~> "typeArgs" ~> "tname" ~>
  "schemaType" <<~ Schemas.requireSchemaType @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "wrapped" <<~ ExtractCore.wrappedType @@ var "tname" @@ var "sbody" $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
  "swrapped" <~ Substitution.substInType @@ var "subst" @@ var "wrapped" $
  produce $ MetaTypes.function
    (Schemas.nominalApplication @@ var "tname" @@ var "typeArgs")
    (var "swrapped")

typeOfVariable :: TBinding (TypeContext -> [Type] -> Name -> Flow s Type)
typeOfVariable = define "typeOfVariable" $
  doc "Reconstruct the type of a variable" $
  "tx" ~> "typeArgs" ~> "name" ~>
  "rawType" <~ Maps.lookup (var "name") (Typing.typeContextTypes $ var "tx") $
  "failMsg" <~ Flows.fail (Strings.cat $ list [
      string "unbound variable: ",
      Core.unName $ var "name",
      string ". Variables: {",
      Strings.intercalate (string ", ") (Lists.map (unaryFunction $ Core.unName) $ Maps.keys $ Typing.typeContextTypes $ var "tx"),
      string "}"]) $
  "t" <<~ optCases (var "rawType")
    (var "failMsg")
    ("t" ~> Logic.ifElse (Lists.null $ var "typeArgs")
      (Schemas.instantiateType @@ var "t")
      (Flows.pure $ var "t")) $
  applyTypeArgumentsToType @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfWrappedTerm :: TBinding (TypeContext -> [Type] -> WrappedTerm -> Flow s Type)
typeOfWrappedTerm = define "typeOfWrappedTerm" $
  doc "Reconstruct the type of a wrapped term" $
  "tx" ~> "typeArgs" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $

  -- The following is only for checking, not for reconstruction
  "body" <~ Core.wrappedTermBody (var "wt") $
  "btype" <<~ typeOf @@ var "tx" @@ noTypeArgs  @@ var "body" $
  exec (checkTypeVariables @@ var "tx" @@ var "btype") $

  produce $ Schemas.nominalApplication @@ var "tname" @@ var "typeArgs"

normalizeTypeFreeVars :: TBinding (Type -> Type)
normalizeTypeFreeVars = define "normalizeTypeFreeVars" $
  doc "Normalize free type variables in a type to canonical names based on order of first occurrence. This allows comparing types that differ only in the naming of free type variables." $
  "typ" ~>
  "collectVars" <~ ("acc" ~> "t" ~>
    cases _Type (var "t")
      (Just $ var "acc") [
      _Type_variable>>: "v" ~>
        Logic.ifElse (Maps.member (var "v") (var "acc"))
          (var "acc")
          (Maps.insert (var "v") (Core.name $ Strings.cat2 (string "_tv") (Literals.showInt32 $ Maps.size $ var "acc")) (var "acc"))]) $
  "subst" <~ Rewriting.foldOverType @@ Coders.traversalOrderPre @@ var "collectVars" @@ Maps.empty @@ var "typ" $
  Rewriting.substituteTypeVariables @@ var "subst" @@ var "typ"

typesAllEffectivelyEqual :: TBinding (TypeContext -> [Type] -> Bool)
typesAllEffectivelyEqual = define "typesAllEffectivelyEqual" $
  doc ("Check whether a list of types are effectively equal, disregarding type aliases and free type variable naming."
    <> " Also treats free type variables (not in schema) as wildcards, since inference has already verified consistency.") $
  "tx" ~> "tlist" ~>
  "types" <~ Typing.inferenceContextSchemaTypes (Typing.typeContextInferenceContext $ var "tx") $
  "containsFreeVar" <~ ("t" ~>
    "allVars" <~ Rewriting.freeVariablesInTypeSimple @@ var "t" $
    "schemaNames" <~ Sets.fromList (Maps.keys $ var "types") $
    Logic.not $ Sets.null $ Sets.difference (var "allVars") (var "schemaNames")) $
  "anyContainsFreeVar" <~ Lists.foldl ("acc" ~> "t" ~> Logic.or (var "acc") (var "containsFreeVar" @@ var "t")) false (var "tlist") $
  Logic.ifElse (var "anyContainsFreeVar")
    true
    (Logic.ifElse (allEqual @@ (Lists.map ("t" ~> normalizeTypeFreeVars @@ var "t") (var "tlist")))
      true
      (allEqual @@ (Lists.map ("t" ~> normalizeTypeFreeVars @@ (Rewriting.deannotateTypeRecursive @@ (Rewriting.replaceTypedefs @@ var "types" @@ var "t"))) (var "tlist"))))

-- | Check if a type contains any type variable that's in scope (from typeContextTypeVariables)
containsInScopeTypeVars :: TBinding (TypeContext -> Type -> Bool)
containsInScopeTypeVars = define "containsInScopeTypeVars" $
  doc "Check if a type contains any type variable from the current scope" $
  "tx" ~> "t" ~>
  "vars" <~ Typing.typeContextTypeVariables (var "tx") $
  "freeVars" <~ Rewriting.freeVariablesInTypeSimple @@ var "t" $
  Logic.not $ Sets.null $ Sets.intersection (var "vars") (var "freeVars")

typesEffectivelyEqual :: TBinding (TypeContext -> Type -> Type -> Bool)
typesEffectivelyEqual = define "typesEffectivelyEqual" $
  doc "Check whether two types are effectively equal, disregarding type aliases, forall quantifiers, and treating in-scope type variables as wildcards" $
  "tx" ~> "t1" ~> "t2" ~>
  -- If either type contains in-scope type variables, treat them as matching
  -- This handles the case where fresh type variables from instantiation haven't been substituted
  Logic.or (containsInScopeTypeVars @@ var "tx" @@ var "t1") $
  Logic.or (containsInScopeTypeVars @@ var "tx" @@ var "t2") $
  typesAllEffectivelyEqual @@ var "tx" @@ list [
    Schemas.fullyStripAndNormalizeType @@ var "t1",
    Schemas.fullyStripAndNormalizeType @@ var "t2"]
