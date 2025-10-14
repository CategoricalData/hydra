{-# LANGUAGE OverloadedStrings #-}

{-
TODO:
* Double-check the use of checkTypeVariablesDef for consistency across rules
* In nominal rules (record/union/wrap intro and elim), double-check that we are checking provided data against the schema
* Use the inference debug flag to enable / disable code which is purely for checking and not for reconstruction.
* When the debug flag is set, call checkType from hydra.inference rules
-}

module Hydra.Sources.Kernel.Terms.Checking where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Constants as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Mantle as ShowMantle
import qualified Hydra.Sources.Kernel.Terms.Substitution as Substitution
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.checking") elements
    [Constants.module_, ExtractCore.module_, Formatting.module_, Lexical.module_,
      Rewriting.module_, Schemas.module_,
      ShowCore.module_, ShowMantle.module_,
      Substitution.module_, Variants.module_]
    kernelTypesModules $
    Just "Type checking and type reconstruction (type-of) for the results of Hydra unification and inference"
  where
    elements = [
      el applyTypeArgumentsToTypeDef,
      el checkForUnboundTypeVariablesDef,
      el checkNominalApplicationDef,
      el checkSameTypeDef,
      el checkTypeDef,
      el checkTypeSubstDef,
      el checkTypeVariablesDef,
      el toFContextDef,
      el typeOfDef,
      el typeOfAnnotatedTermDef,
      el typeOfApplicationDef,
      el typeOfCaseStatementDef,
      el typeOfInjectionDef,
      el typeOfLambdaDef,
      el typeOfLetDef,
      el typeOfListDef,
      el typeOfLiteralDef,
      el typeOfMapDef,
      el typeOfOptionalDef,
      el typeOfPrimitiveDef,
      el typeOfProjectionDef,
      el typeOfRecordDef,
      el typeOfSetDef,
      el typeOfTupleDef,
      el typeOfTupleProjectionDef,
      el typeOfTypeApplicationDef,
      el typeOfTypeLambdaDef,
      el typeOfUnitDef,
      el typeOfUnwrapDef,
      el typeOfVariableDef,
      el typeOfWrappedTermDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

--

applyTypeArgumentsToTypeDef :: TBinding (TypeContext -> [Type] -> Type -> Flow s Type)
applyTypeArgumentsToTypeDef = define "applyTypeArgumentsToType" $
  "tx" ~> "typeArgs" ~> "t" ~>
  exec (ref checkTypeVariablesDef @@ var "tx" @@ var "t") $
  Logic.ifElse (Lists.null $ var "typeArgs")
    (produce $ var "t")
    (cases _Type (var "t")
      (Just $ Flows.fail $ Strings.cat $ list [
        string "not a forall type: ",
        ref ShowCore.typeDef @@ var "t"]) [
      _Type_forall>>: "ft" ~>
        "v" <~ Core.forallTypeParameter (var "ft") $
        "tbody" <~ Core.forallTypeBody (var "ft") $
        ref applyTypeArgumentsToTypeDef
          @@ var "tx"
          @@ (Lists.tail $ var "typeArgs")
          @@ (ref Substitution.substInTypeDef
            @@ (Typing.typeSubst $ Maps.singleton (var "v") (Lists.head $ var "typeArgs"))
            @@ (var "tbody"))])

checkForUnboundTypeVariablesDef :: TBinding (InferenceContext -> Term -> Flow s ())
checkForUnboundTypeVariablesDef = define "checkForUnboundTypeVariables" $
  doc "Check that a term has no unbound type variables" $
  "cx" ~> "term0" ~>
  "svars" <~ Sets.fromList (Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "checkRecursive" <~ ("vars" ~> "trace" ~> "lbinding" ~> "term" ~>
    "recurse" <~ var "checkRecursive" @@ var "vars" @@ var "trace" @@ var "lbinding" $
    "dflt" <~ (
      exec (Flows.mapList (var "recurse") (ref Rewriting.subtermsDef @@ var "term")) $
      produce unit) $
    "check" <~ ("typ" ~>
      "freevars" <~ ref Rewriting.freeVariablesInTypeDef @@ var "typ" $
      "badvars" <~ Sets.difference (Sets.difference (var "freevars") (var "vars")) (var "svars") $
      Logic.ifElse (Sets.null $ var "badvars")
        (produce unit)
        (Flows.fail $ "unbound type variables: {"
          ++ (Strings.intercalate ", " (Lists.map (unaryFunction Core.unName) $ Sets.toList $ var "badvars"))
          ++ "} in type " ++ (ref ShowCore.typeDef @@ var "typ") ++ " at path: " ++ (Strings.intercalate " >> " (Lists.reverse $ var "trace"))
          ++ (optCases (var "lbinding")
            ("none")
            ("binding" ~>
                 ". bound term = " ++ (ref ShowCore.termDef @@ (Core.bindingTerm $ var "binding"))
              ++ ". bound type = " ++ (optCases (Core.bindingType $ var "binding") ("none") (ref ShowCore.typeSchemeDef)))))) $
    "checkOptional" <~ ("m" ~>
      exec (Flows.mapOptional (var "check") (var "m")) $
      produce unit) $
    "checkOptionalList" <~ ("ml" ~>
      exec (Flows.mapOptional ("l" ~> Flows.mapList (var "check") (var "l")) (var "ml")) $
      produce unit) $
    cases _Term (var "term")
      (Just $ var "dflt") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "dflt") [
        _Function_elimination>>: "e" ~> cases _Elimination (var "e")
          (Just $ var "dflt") [
          _Elimination_product>>: "tp" ~> var "checkOptionalList" @@ (Core.tupleProjectionDomain $ var "tp")],
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
  var "checkRecursive" @@ Sets.empty @@ list ["top level"] @@ nothing @@ var "term0"

checkNominalApplicationDef ::  TBinding (TypeContext -> Name -> [Type] -> Flow s ())
checkNominalApplicationDef = define "checkNominalApplication" $
  "tx" ~> "tname" ~> "typeArgs" ~>
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "vars" <~ Core.typeSchemeVariables (var "schemaType") $
  "body" <~ Core.typeSchemeType (var "schemaType") $
  "varslen" <~ Lists.length (var "vars") $
  "argslen" <~ Lists.length (var "typeArgs") $
  Logic.ifElse (Equality.equal (var "varslen") (var "argslen"))
    (produce unit)
    (Flows.fail $ "nominal type " ++ Core.unName (var "tname") ++ " applied to the wrong number of type arguments: "
      ++ "(expected " ++ Literals.showInt32 (var "varslen") ++ " arguments, got "
      ++ Literals.showInt32 (var "argslen") ++ "): "
      ++ (ref Formatting.showListDef @@ (ref ShowCore.typeDef) @@ (var "typeArgs")))

checkSameTypeDef :: TBinding (String -> [Type] -> Flow s Type)
checkSameTypeDef = define "checkSameType" $
  doc "Ensure all types in a list are equal and return the common type" $
  "desc" ~> "types" ~>
  "h" <~ Lists.head (var "types") $
  "allEqual" <~ Lists.foldl
    ("b" ~> "t" ~> Logic.and (var "b") (Equality.equal (var "t") (var "h")))
    true
    (var "types") $
  Logic.ifElse (var "allEqual")
    (Flows.pure $ var "h")
    (Flows.fail $ Strings.cat $ list [
      string "unequal types ",
      (ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "types"),
      string " in ",
      var "desc"])

-- TODO: unused
checkTypeDef :: TBinding (TypeContext -> Term -> Type -> Flow s ())
checkTypeDef = define "checkType" $
  doc "Check that a term has the expected type" $
  "tx" ~> "term" ~> "typ" ~>
  "cx" <~ Typing.typeContextInferenceContext (var "tx") $
  "vars" <~ Typing.typeContextVariables (var "tx") $
  Logic.ifElse (ref Constants.debugInferenceDef)
    ("t0" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "term" $
      Logic.ifElse (Equality.equal (var "t0") (var "typ"))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "type checking failed: expected ",
          ref ShowCore.typeDef @@ var "typ",
          string " but found ",
          ref ShowCore.typeDef @@ var "t0"]))
    (Flows.pure unit)

checkTypeSubstDef :: TBinding (InferenceContext -> TypeSubst -> Flow s TypeSubst)
checkTypeSubstDef = define "checkTypeSubst" $
  doc ("Sanity-check a type substitution arising from unification. Specifically, check that schema types have not been"
    <> " inappropriately unified with type variables inferred from terms.") $
  "cx" ~> "subst" ~>
  "s" <~ Typing.unTypeSubst (var "subst") $
  "vars" <~ Sets.fromList (Maps.keys $ var "s") $
  "suspectVars" <~ Sets.intersection (var "vars") (Sets.fromList $ Maps.keys $ Typing.inferenceContextSchemaTypes $ var "cx") $
  "isNominal" <~ ("ts" ~> cases _Type (ref Rewriting.deannotateTypeDef @@ (Core.typeSchemeType $ var "ts"))
    (Just false) [
    _Type_record>>: constant true,
    _Type_union>>: constant true,
    _Type_wrap>>: constant true]) $
  "badVars" <~ Sets.fromList (Lists.filter
    ("v" ~> Optionals.maybe false (var "isNominal") $
      ref Lexical.dereferenceSchemaTypeDef @@ var "v" @@ (Typing.inferenceContextSchemaTypes $ var "cx"))
    (Sets.toList $ var "suspectVars")) $
  "badPairs" <~ Lists.filter ("p" ~> Sets.member (first $ var "p") (var "badVars")) (Maps.toList $ var "s") $
  "printPair" <~ ("p" ~> (Core.unName $ first $ var "p") ++ " --> " ++ (ref ShowCore.typeDef @@ second (var "p"))) $
  Logic.ifElse (Sets.null $ var "badVars")
    (produce $ var "subst")
    (Flows.fail $ "Schema type(s) incorrectly unified: {"
      ++ (Strings.intercalate ", " (Lists.map (var "printPair") (var "badPairs")))
      ++ "}")

-- W: wfTy
checkTypeVariablesDef :: TBinding (TypeContext -> Type -> Flow s ())
checkTypeVariablesDef = define "checkTypeVariables" $
  doc "Check that all type variables in a type are bound" $
  "tx" ~> "typ" ~>
  "cx" <~ Typing.typeContextInferenceContext (var "tx") $
  "vars" <~ Typing.typeContextVariables (var "tx") $
  trace (Strings.cat $ list [
    string "checking variables of: ",
    ref ShowCore.typeDef @@ var "typ"]) $
  cases _Type (var "typ")
    (Just $
      "result" <<~ Flows.mapList (ref checkTypeVariablesDef @@ var "tx")
        (ref Rewriting.subtypesDef @@ var "typ") $
      Flows.pure unit) [
    _Type_forall>>: "ft" ~> ref checkTypeVariablesDef
      @@ (Typing.typeContextWithVariables (var "tx") (Sets.insert (Core.forallTypeParameter $ var "ft") (var "vars")))
      @@ (Core.forallTypeBody $ var "ft"),
    _Type_variable>>: "v" ~> Logic.ifElse (Sets.member (var "v") (var "vars"))
      (Flows.pure unit)
      (Logic.ifElse (Maps.member (var "v") (Typing.inferenceContextSchemaTypes (var "cx")))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "unbound type variable \"",
          Core.unName $ var "v",
          string "\" in ",
          ref ShowCore.typeDef @@ var "typ"]))]

-- TODO: unused (?). This function can be used to construct Typing.typeContextTypes from Typing.typeContextInferenceContext
toFContextDef :: TBinding (InferenceContext -> M.Map Name Type)
toFContextDef = define "toFContext" $
  doc "Convert inference context to type context" $
  "cx" ~> Maps.map (ref Schemas.typeSchemeToFTypeDef) $ Typing.inferenceContextDataTypes $ var "cx"

typeOfDef :: TBinding (TypeContext -> [Type] -> Term -> Flow s Type)
typeOfDef = define "typeOf" $
  doc "Given a type context, reconstruct the type of a System F term" $
  "tx" ~> "typeArgs" ~> "term" ~>

  trace (Strings.cat $ list [
    string "checking type of: ",
    ref ShowCore.termDef @@ var "term",
    string " (vars: ",
    ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Sets.toList $ Typing.typeContextVariables $ var "tx"),
    string ", typeArgs: ",
    ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "typeArgs",
    string ", types: ",
--    ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Maps.keys $ Typing.typeContextTypes $ var "tx"),
    ref Formatting.showListDef @@ ("p" ~> Core.unName (first $ var "p") ++ ": " ++ (ref ShowCore.typeDef @@ (second $ var "p"))) @@ (Maps.toList $ Typing.typeContextTypes $ var "tx"),
    string ")"]) $

  cases _Term (var "term")
    (Just $ Flows.fail $ Strings.cat $ list [
      "unsupported term variant in typeOf: ",
      ref ShowMantle.termVariantDef @@ (ref Variants.termVariantDef @@ var "term")]) [
    _Term_annotated>>: ref typeOfAnnotatedTermDef @@ var "tx" @@ var "typeArgs",
    _Term_application>>: ref typeOfApplicationDef @@ var "tx" @@ var "typeArgs",
    _Term_function>>: "f" ~>
      cases _Function (var "f") Nothing [
        _Function_elimination>>: "elm" ~>
          cases _Elimination (var "elm") Nothing [
            _Elimination_product>>: ref typeOfTupleProjectionDef @@ var "tx" @@ var "typeArgs",
            _Elimination_record>>: ref typeOfProjectionDef @@ var "tx" @@ var "typeArgs",
            _Elimination_union>>: ref typeOfCaseStatementDef @@ var "tx" @@ var "typeArgs",
            _Elimination_wrap>>: ref typeOfUnwrapDef @@ var "tx" @@ var "typeArgs"],
        _Function_lambda>>: ref typeOfLambdaDef @@ var "tx" @@ var "typeArgs",
        _Function_primitive>>: ref typeOfPrimitiveDef @@ var "tx" @@ var "typeArgs"],
    _Term_let>>: ref typeOfLetDef @@ var "tx" @@ var "typeArgs",
    _Term_list>>: ref typeOfListDef @@ var "tx" @@ var "typeArgs",
    _Term_literal>>: ref typeOfLiteralDef @@ var "tx" @@ var "typeArgs",
    _Term_map>>: ref typeOfMapDef @@ var "tx" @@ var "typeArgs",
    _Term_optional>>: ref typeOfOptionalDef @@ var "tx" @@ var "typeArgs",
    _Term_product>>: ref typeOfTupleDef @@ var "tx" @@ var "typeArgs",
    _Term_record>>: ref typeOfRecordDef @@ var "tx" @@ var "typeArgs",
    _Term_set>>: ref typeOfSetDef @@ var "tx" @@ var "typeArgs",
    -- TermSum (Sum idx size term1) is ignored for now. See https://github.com/CategoricalData/hydra/issues/134.
    _Term_typeApplication>>: ref typeOfTypeApplicationDef @@ var "tx" @@ var "typeArgs",
    _Term_typeLambda>>: ref typeOfTypeLambdaDef @@ var "tx" @@ var "typeArgs",
    _Term_union>>: ref typeOfInjectionDef @@ var "tx" @@ var "typeArgs",
    _Term_unit>>: constant $ ref typeOfUnitDef @@ var "tx" @@ var "typeArgs",
    _Term_variable>>: ref typeOfVariableDef @@ var "tx" @@ var "typeArgs",
    _Term_wrap>>: ref typeOfWrappedTermDef @@ var "tx" @@ var "typeArgs"]

typeOfAnnotatedTermDef :: TBinding (TypeContext -> [Type] -> AnnotatedTerm -> Flow s Type)
typeOfAnnotatedTermDef = define "typeOfAnnotatedTerm" $
  "tx" ~> "typeArgs" ~> "at" ~>
  ref typeOfDef @@ var "tx" @@ var "typeArgs" @@ Core.annotatedTermBody (var "at")

typeOfApplicationDef :: TBinding (TypeContext -> [Type] -> Application -> Flow s Type)
typeOfApplicationDef = define "typeOfApplication" $
  "tx" ~> "typeArgs" ~> "app" ~>
  "fun" <~ Core.applicationFunction (var "app") $
  "arg" <~ Core.applicationArgument (var "app") $
  "tfun" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "fun" $
  "targ" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "arg" $
  exec (ref checkTypeVariablesDef @@ var "tx" @@ var "tfun") $
  exec (ref checkTypeVariablesDef @@ var "tx" @@ var "targ") $
  "tryType" <~ ("t" ~> cases _Type (var "t")
    (Just $ Flows.fail $ Strings.cat $ list [
      "left hand side of application ",
      ref ShowCore.termDef @@ var "fun",
      " is not a function type: ",
      ref ShowCore.typeDef @@ var "t"]) [
    -- These forall types can arise from bindUnboundTypeVariables
    _Type_forall>>: "ft" ~> var "tryType" @@ (Core.forallTypeBody (var "ft")),
    _Type_function>>: "ft" ~>
      "dom" <~ Core.functionTypeDomain (var "ft") $
      "cod" <~ Core.functionTypeCodomain (var "ft") $
      Logic.ifElse (Equality.equal (var "dom") (var "targ"))
        (Flows.pure $ var "cod")
        (Flows.fail $ Strings.cat $ list [
          "expected ",
          ref ShowCore.typeDef @@ var "dom",
          " but found ",
          ref ShowCore.typeDef @@ var "targ"])]) $
  "t" <<~ var "tryType" @@ var "tfun" $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfCaseStatementDef :: TBinding (TypeContext -> [Type] -> CaseStatement -> Flow s Type)
typeOfCaseStatementDef = define "typeOfCaseStatement" $
  "tx" ~> "typeArgs" ~> "cs" ~>
  "tname" <~ Core.caseStatementTypeName (var "cs") $

  "dflt" <~ Core.caseStatementDefault (var "cs") $
  "cases" <~ Core.caseStatementCases (var "cs") $
  "cterms" <~ Lists.map (unaryFunction Core.fieldTerm) (var "cases") $
  "tdflt" <<~ Flows.mapOptional ("e" ~> ref typeOfDef @@ var "tx" @@ list [] @@ var "e") (var "dflt") $
  "tcterms" <<~ Flows.mapList ("e" ~> ref typeOfDef @@ var "tx" @@ list [] @@ var "e") (var "cterms") $
  "fcods" <<~ Flows.mapList ("t" ~> Flows.map (unaryFunction Core.functionTypeCodomain) $ ref ExtractCore.functionTypeDef @@ var "t") (var "tcterms") $
  "cods" <~ Optionals.cat (Lists.cons (var "tdflt") $ Lists.map (unaryFunction Optionals.pure) (var "fcods")) $
  "cod" <<~ ref checkSameTypeDef @@ string "case branches" @@ var "cods" $
--  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
--  "scod" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "cod" $

  produce $ Core.typeFunction $ Core.functionType
    (ref Schemas.nominalApplicationDef @@ var "tname"  @@ var "typeArgs")
    (var "cod")

typeOfInjectionDef :: TBinding (TypeContext -> [Type] -> Injection -> Flow s Type)
typeOfInjectionDef = define "typeOfInjection" $
  "tx" ~> "typeArgs" ~> "injection" ~>
  "tname" <~ Core.injectionTypeName (var "injection") $

  -- The following is only for checking, not for reconstruction
  "field" <~ Core.injectionField (var "injection") $
  "fname" <~ Core.fieldName (var "field") $
  "fterm" <~ Core.fieldTerm (var "field") $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "sbody" $
  "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $

  produce $ ref Schemas.nominalApplicationDef @@ var "tname"  @@ var "typeArgs"

typeOfLambdaDef :: TBinding (TypeContext -> [Type] -> Lambda -> Flow s Type)
typeOfLambdaDef = define "typeOfLambda" $
  "tx" ~> "typeArgs" ~> "l" ~>
  "v" <~ Core.lambdaParameter (var "l") $
  "mdom" <~ Core.lambdaDomain (var "l") $
  "body" <~ Core.lambdaBody (var "l") $
  "tbody" <<~ optCases (var "mdom")
    (Flows.fail "untyped lambda")
    ("dom" ~>
      exec (ref checkTypeVariablesDef @@ var "tx" @@ var "dom") $
      "types2" <~ Maps.insert (var "v") (var "dom") (Typing.typeContextTypes $ var "tx") $
      "cod" <<~ ref typeOfDef @@ (Typing.typeContextWithTypes (var "tx") $ var "types2") @@ list [] @@ var "body" $
      exec (ref checkTypeVariablesDef @@ var "tx" @@ var "cod") $
      Flows.pure $ Core.typeFunction $ Core.functionType (var "dom") (var "cod")) $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "tbody"

typeOfLetDef :: TBinding (TypeContext -> [Type] -> Let -> Flow s Type)
typeOfLetDef = define "typeOfLet" $
  "tx" ~> "typeArgs" ~> "letTerm" ~>
  "bs" <~ Core.letBindings (var "letTerm") $
  "body" <~ Core.letBody (var "letTerm") $
  "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bs") $
  "bterms" <~ Lists.map (unaryFunction Core.bindingTerm) (var "bs") $
  "bindingType" <~ ("b" ~>
    Optionals.maybe
      (Flows.fail $ Strings.cat $ list [
        string "untyped let binding: ",
        ref ShowCore.bindingDef @@ var "b"])
      ("ts" ~> Flows.pure $ ref Schemas.typeSchemeToFTypeDef @@ var "ts")
      (Core.bindingType $ var "b")) $
  -- Previously inferred types for each binding
  "btypes" <<~ Flows.mapList (var "bindingType") (var "bs") $
  -- Extended type context (note: new bindings override old ones)
  "tx2" <~ (Typing.typeContextWithTypes (var "tx")
    (Maps.union
      (Maps.fromList $ Lists.zip (var "bnames") (var "btypes"))
      (Typing.typeContextTypes $ var "tx"))) $
  -- Reconstructed type for each binding
  "typeofs" <<~ Flows.mapList (ref typeOfDef @@ var "tx2" @@ list []) (var "bterms") $
  exec (Flows.mapList (ref checkTypeVariablesDef @@ var "tx") (var "btypes")) $
  exec (Flows.mapList (ref checkTypeVariablesDef @@ var "tx") (var "typeofs")) $
  "t" <<~ Logic.ifElse (Equality.equal (var "typeofs") (var "btypes"))
    (ref typeOfDef @@ var "tx2" @@ list [] @@ var "body")
    (Flows.fail $ Strings.cat $ list [
      string "binding types disagree: ",
      ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "btypes",
      string " and ",
      ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "typeofs",
      string " from terms: ",
      ref Formatting.showListDef @@ ref ShowCore.termDef @@ var "bterms"]) $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfListDef :: TBinding (TypeContext -> [Type] -> [Term] -> Flow s Type)
typeOfListDef = define "typeOfList" $
  "tx" ~> "typeArgs" ~> "els" ~>
  Logic.ifElse (Lists.null $ var "els")
    -- Empty list is polymorphic
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 1))
      (Flows.pure $ Core.typeList $ Lists.head $ var "typeArgs")
      (Flows.fail $ "list type applied to more or less than one argument"))
    -- Nonempty list must have elements of the same type
    ( "eltypes" <<~ Flows.mapList
       (ref typeOfDef @@ var "tx" @@ list [])
       (var "els") $
       "unifiedType" <<~ ref checkSameTypeDef @@ string "list elements" @@ var "eltypes" $
       -- Verify the unified type is well-formed in the current scope
       exec (ref checkTypeVariablesDef @@ var "tx" @@ var "unifiedType") $
       Flows.pure $ Core.typeList $ var "unifiedType")

typeOfLiteralDef :: TBinding (TypeContext -> [Type] -> Literal -> Flow s Type)
typeOfLiteralDef = define "typeOfLiteral" $
  "tx" ~> "typeArgs" ~> "lit" ~>
  "t" <~ Core.typeLiteral (ref Variants.literalTypeDef @@ var "lit") $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfMapDef :: TBinding (TypeContext -> [Type] -> M.Map Term Term -> Flow s Type)
typeOfMapDef = define "typeOfMap" $
  "tx" ~> "typeArgs" ~> "m" ~>
  Logic.ifElse (Maps.null $ var "m")
    -- Empty map is polymorphic
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 2))
      (Flows.pure $ Core.typeMap $ Core.mapType
        (Lists.at (int32 0) $ var "typeArgs")
        (Lists.at (int32 1) $ var "typeArgs"))
      (Flows.fail $ "map type applied to more or less than two arguments"))
    -- Nonempty map must have keys and values of the same type
    ( "t" <<~ (
        "pairs" <~ Maps.toList (var "m") $
        "kt" <<~ Flows.bind
          (Flows.mapList (ref typeOfDef @@ var "tx" @@ list []) $
            Lists.map (unaryFunction first) (var "pairs"))
          (ref checkSameTypeDef @@ string "map keys") $
        "vt" <<~ Flows.bind
          (Flows.mapList (ref typeOfDef @@ var "tx" @@ list []) $
            Lists.map (unaryFunction second) (var "pairs"))
          (ref checkSameTypeDef @@ string "map values") $
        exec (ref checkTypeVariablesDef @@ var "tx" @@ var "kt") $
        exec (ref checkTypeVariablesDef @@ var "tx" @@ var "vt") $
        Flows.pure $ Core.typeMap $ Core.mapType (var "kt") (var "vt")) $
      ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t")

typeOfOptionalDef :: TBinding (TypeContext -> [Type] -> Maybe Term -> Flow s Type)
typeOfOptionalDef = define "typeOfOptional" $
  "tx" ~> "typeArgs" ~> "mt" ~>
  optCases (var "mt")
    -- Nothing case is polymorphic
    ("n" <~ Lists.length (var "typeArgs") $
      Logic.ifElse (Equality.equal (var "n") (int32 1))
        (Flows.pure $ Core.typeOptional $ Lists.head $ var "typeArgs")
        (Flows.fail $ "optional type applied to " ++ Literals.showInt32 (var "n") ++ " argument(s). Expected 1."))
    -- Just case: infer type of the contained term
    ( "term" ~>
      "t" <<~ (
        "termType" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "term" $
         exec (ref checkTypeVariablesDef @@ var "tx" @@ var "termType") $
         Flows.pure $ Core.typeOptional $ var "termType") $
      ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t")

typeOfPrimitiveDef :: TBinding (TypeContext -> [Type] -> Name -> Flow s Type)
typeOfPrimitiveDef = define "typeOfPrimitive" $
  "tx" ~> "typeArgs" ~> "name" ~>
  "ts" <<~ optCases
    (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ Typing.typeContextInferenceContext $ var "tx"))
    (Flows.fail $ Strings.cat $ list [
      string "no such primitive: ",
      Core.unName $ var "name"])
    (ref Schemas.instantiateTypeSchemeDef) $
  "t" <~ ref Schemas.typeSchemeToFTypeDef @@ var "ts" $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfProjectionDef :: TBinding (TypeContext -> [Type] -> Projection -> Flow s Type)
typeOfProjectionDef = define "typeOfProjection" $
  "tx" ~> "typeArgs" ~> "p" ~>
  "tname" <~ Core.projectionTypeName (var "p") $
  "fname" <~ Core.projectionField (var "p") $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "sfields" <<~ ref ExtractCore.recordTypeDef @@ var "tname" @@ var "sbody" $
  "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
  "sftyp" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "ftyp" $
  produce $ Core.typeFunction $ Core.functionType
    (ref Schemas.nominalApplicationDef @@ var "tname" @@ var "typeArgs")
    (var "sftyp")

typeOfRecordDef :: TBinding (TypeContext -> [Type] -> Record -> Flow s Type)
typeOfRecordDef = define "typeOfRecord" $
  "tx" ~> "typeArgs" ~> "record" ~>
  "tname" <~ Core.recordTypeName (var "record") $

  -- The following is only for checking, not for reconstruction
  "fields" <~ Core.recordFields (var "record") $
  "ftypes" <<~ Flows.mapList
    (ref typeOfDef @@ var "tx" @@ list [])
    (Lists.map (unaryFunction Core.fieldTerm) (var "fields")) $
  exec (Flows.mapList (ref checkTypeVariablesDef @@ var "tx") (var "ftypes")) $

  produce $ ref Schemas.nominalApplicationDef @@ var "tname" @@ var "typeArgs"

typeOfSetDef :: TBinding (TypeContext -> [Type] -> S.Set Term -> Flow s Type)
typeOfSetDef = define "typeOfSet" $
  "tx" ~> "typeArgs" ~> "els" ~>
  Logic.ifElse (Sets.null $ var "els")
    -- Empty set is polymorphic
    (Logic.ifElse (Equality.equal (Lists.length $ var "typeArgs") (int32 1))
      (Flows.pure $ Core.typeSet $ Lists.head $ var "typeArgs")
      (Flows.fail $ "set type applied to more or less than one argument"))
    -- Nonempty set must have elements of the same type
    ( "eltypes" <<~ Flows.mapList
       (ref typeOfDef @@ var "tx" @@ list [])
       (Sets.toList $ var "els") $
       "unifiedType" <<~ ref checkSameTypeDef @@ string "set elements" @@ var "eltypes" $
       -- Verify the unified type is well-formed in the current scope
       exec (ref checkTypeVariablesDef @@ var "tx" @@ var "unifiedType") $
       produce $ Core.typeSet $ var "unifiedType")

typeOfTupleDef :: TBinding (TypeContext -> [Type] -> [Term] -> Flow s Type)
typeOfTupleDef = define "typeOfTuple" $
  "tx" ~> "typeArgs" ~> "tuple" ~>
  "etypes" <<~ Flows.mapList (ref typeOfDef @@ var "tx" @@ list []) (var "tuple") $
  exec (Flows.mapList (ref checkTypeVariablesDef @@ var "tx") (var "etypes")) $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ (Core.typeProduct $ var "etypes")

typeOfTupleProjectionDef :: TBinding (TypeContext -> [Type] -> TupleProjection -> Flow s Type)
typeOfTupleProjectionDef = define "typeOfTupleProjection" $
  "tx" ~> "typeArgs" ~> "tp" ~>
  "t" <<~ (
    "index" <~ Core.tupleProjectionIndex (var "tp") $
    "arity" <~ Core.tupleProjectionArity (var "tp") $
    "mtypes" <~ Core.tupleProjectionDomain (var "tp") $
    Optionals.maybe
      (Flows.fail "untyped tuple projection")
      ( "types" ~>
        exec (Flows.mapList (ref checkTypeVariablesDef @@ var "tx") (var "types")) $
        Flows.pure $ Core.typeFunction $ Core.functionType
          (Core.typeProduct $ var "types")
          (Lists.at (var "index") (var "types")))
      (var "mtypes")) $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfTypeApplicationDef :: TBinding (TypeContext -> [Type] -> TypeApplicationTerm -> Flow s Type)
typeOfTypeApplicationDef = define "typeOfTypeApplication" $
  "tx" ~> "typeArgs" ~> "tyapp" ~>
  "body" <~ Core.typeApplicationTermBody (var "tyapp") $
  "t" <~ Core.typeApplicationTermType (var "tyapp") $
  ref typeOfDef @@ var "tx" @@ Lists.cons (var "t") (var "typeArgs") @@ var "body"

typeOfTypeLambdaDef :: TBinding (TypeContext -> [Type] -> TypeLambda -> Flow s Type)
typeOfTypeLambdaDef = define "typeOfTypeLambda" $
  "tx" ~> "typeArgs" ~> "tl" ~>
  "v" <~ Core.typeLambdaParameter (var "tl") $
  "body" <~ Core.typeLambdaBody (var "tl") $
  "vars" <~ Typing.typeContextVariables (var "tx") $
  "tx2" <~ Typing.typeContextWithVariables (var "tx") (Sets.insert (var "v") (var "vars")) $
  "t1" <<~ ref typeOfDef @@ var "tx2" @@ list [] @@ var "body" $
  exec (ref checkTypeVariablesDef @@ var "tx2" @@ var "t1") $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs"
    @@ (Core.typeForall $ Core.forallType (var "v") (var "t1"))

typeOfUnitDef :: TBinding (TypeContext -> [Type] -> Flow s Type)
typeOfUnitDef = define "typeOfUnit" $
  "tx" ~> "typeArgs" ~>
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ Core.typeUnit

typeOfUnwrapDef :: TBinding (TypeContext -> [Type] -> Name -> Flow s Type)
typeOfUnwrapDef = define "typeOfUnwrap" $
  "tx" ~> "typeArgs" ~> "tname" ~>
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ Typing.typeContextInferenceContext (var "tx") @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "sbody" <~ Core.typeSchemeType (var "schemaType") $
  "wrapped" <<~ ref ExtractCore.wrappedTypeDef @@ var "tname" @@ var "sbody" $
  "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "typeArgs")) $
  "swrapped" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "wrapped" $
  produce $ TTypes.function
    (ref Schemas.nominalApplicationDef @@ var "tname" @@ var "typeArgs")
    (var "swrapped")

typeOfVariableDef :: TBinding (TypeContext -> [Type] -> Name -> Flow s Type)
typeOfVariableDef = define "typeOfVariable" $
  "tx" ~> "typeArgs" ~> "name" ~>
  "t" <<~ optCases (Maps.lookup (var "name") (Typing.typeContextTypes $ var "tx"))
    (Flows.fail $ Strings.cat $ list [
      string "unbound variable: ",
      Core.unName $ var "name"])
    (ref Schemas.instantiateTypeDef) $
  ref applyTypeArgumentsToTypeDef @@ var "tx" @@ var "typeArgs" @@ var "t"

typeOfWrappedTermDef :: TBinding (TypeContext -> [Type] -> WrappedTerm -> Flow s Type)
typeOfWrappedTermDef = define "typeOfWrappedTerm" $
  "tx" ~> "typeArgs" ~> "wt" ~>
  "tname" <~ Core.wrappedTermTypeName (var "wt") $

  -- The following is only for checking, not for reconstruction
  "body" <~ Core.wrappedTermBody (var "wt") $
  "btype" <<~ ref typeOfDef @@ var "tx" @@ list [] @@ var "body" $
  exec (ref checkTypeVariablesDef @@ var "tx" @@ var "btype") $

  produce $ ref Schemas.nominalApplicationDef @@ var "tname" @@ var "typeArgs"
