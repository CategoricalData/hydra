{-# LANGUAGE OverloadedStrings #-}

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
import qualified Hydra.Sources.Kernel.Terms.Unification as Unification
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.checking") elements
    [Constants.module_, ExtractCore.module_, Formatting.module_, Lexical.module_,
      Rewriting.module_, Schemas.module_,
      ShowCore.module_, ShowMantle.module_,
      Substitution.module_, Unification.module_, Variants.module_]
    kernelTypesModules $
    Just "Type checking and type reconstruction (type-of) for the results of Hydra unification and inference"
  where
    elements = [
      el checkForUnboundTypeVariablesDef,
      el checkSameTypeDef,
      el checkTypeDef,
      el checkTypeSubstDef,
      el checkTypeVariablesDef,
      el toFContextDef,
      el typeOfDef,
      el typeOfInternalDef,
      el typeOfNominalDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

--

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
              ++ ". bound type = " ++ (optCases (Core.bindingType $ var "binding") ("none") (ref ShowCore.typeSchemeDef))))
--          ++ ". full term = " ++ (ref ShowCore.termDef @@ var "term0")
--          ++ ". vars = {" ++ (Strings.intercalate ", " (Lists.map (unaryFunction Core.unName) $ Sets.toList $ var "vars")) ++ "}"
          )) $
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
        var "recurse" @@ (Core.letEnvironment $ var "l"),
      _Term_typeApplication>>: "tt" ~>
        exec (var "check" @@ (Core.typedTermType $ var "tt")) $
        var "recurse" @@ (Core.typedTermTerm $ var "tt"),
      _Term_typeLambda>>: "tl" ~>
        exec (var "check" @@ (Core.typeVariable $ Core.typeLambdaParameter $ var "tl")) $
        var "recurse" @@ (Core.typeLambdaBody $ var "tl")]) $
  var "checkRecursive" @@ Sets.empty @@ list ["top level"] @@ nothing @@ var "term0"

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

checkTypeDef :: TBinding (S.Set Name -> InferenceContext -> Type -> Term -> Flow s ())
checkTypeDef = define "checkType" $
  doc "Check that a term has the expected type" $
  "k" ~> "g" ~> "t" ~> "e" ~>
  Logic.ifElse (ref Constants.debugInferenceDef)
    ("t0" <<~ ref typeOfInternalDef @@ var "g" @@ var "k" @@ (ref toFContextDef @@ var "g") @@ list [] @@ var "e" $
      Logic.ifElse (Equality.equal (var "t0") (var "t"))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "type checking failed: expected ",
          ref ShowCore.typeDef @@ var "t",
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
checkTypeVariablesDef :: TBinding (InferenceContext -> S.Set Name -> Type -> Flow s ())
checkTypeVariablesDef = define "checkTypeVariables" $
  doc "Check that all type variables in a type are bound" $
  "cx" ~> "tyvars" ~> "typ" ~>
  trace (Strings.cat $ list [
    string "checking variables of: ",
    ref ShowCore.typeDef @@ var "typ"]) $
  cases _Type (var "typ")
    (Just $
      "result" <<~ Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "tyvars")
        (ref Rewriting.subtypesDef @@ var "typ") $
      Flows.pure unit) [
    _Type_forall>>: "ft" ~> ref checkTypeVariablesDef
      @@ var "cx"
      @@ (Sets.insert (Core.forallTypeParameter $ var "ft") (var "tyvars"))
      @@ (Core.forallTypeBody $ var "ft"),
    _Type_variable>>: "v" ~> Logic.ifElse (Sets.member (var "v") (var "tyvars"))
      (Flows.pure unit)
      (Logic.ifElse (Maps.member (var "v") (Typing.inferenceContextSchemaTypes (var "cx")))
        (Flows.pure unit)
        (Flows.fail $ Strings.cat $ list [
          string "unbound type variable \"",
          Core.unName $ var "v",
          string "\" in ",
          ref ShowCore.typeDef @@ var "typ"]))]

toFContextDef :: TBinding (InferenceContext -> M.Map Name Type)
toFContextDef = define "toFContext" $
  doc "Convert inference context to type context" $
  "cx" ~> Maps.map (ref Schemas.typeSchemeToFTypeDef) $ Typing.inferenceContextDataTypes $ var "cx"

typeOfDef :: TBinding (TypeContext -> Term -> Flow s Type)
typeOfDef = define "typeOf" $
  doc "Given a type context, reconstruct the type of a System F term" $
  "tcontext" ~> "term" ~>
  ref typeOfInternalDef @@
    Typing.typeContextInferenceContext (var "tcontext") @@
    Typing.typeContextVariables (var "tcontext") @@
    Typing.typeContextTypes (var "tcontext") @@
    (list []) @@
    var "term"

-- W: typeOf
typeOfInternalDef :: TBinding (InferenceContext -> S.Set Name -> M.Map Name Type -> [Type] -> Term -> Flow s Type)
typeOfInternalDef = define "typeOfInternal" $
  doc "Given internal context, reconstruct the type of a System F term" $
  "cx" ~> "vars" ~> "types" ~> "apptypes" ~> "term" ~>
  "checkApp" <~ ("e" ~> Logic.ifElse (Lists.null $ var "apptypes")
    (var "e")
    ( "app" <~ ("t" ~> "apptypes" ~>
        Logic.ifElse (Lists.null $ var "apptypes")
          (Flows.pure $ var "t")
          (cases _Type (var "t")
            (Just $ Flows.fail $ Strings.cat $ list [
              string "not a forall type: ",
              ref ShowCore.typeDef @@ var "t",
              string " in ",
              ref ShowCore.termDef @@ var "term"]) [
            _Type_forall>>: "ft" ~>
              "v" <~ Core.forallTypeParameter (var "ft") $
              "t2" <~ Core.forallTypeBody (var "ft") $
              var "app"
                @@ (ref Substitution.substInTypeDef @@
                  (Typing.typeSubst $ Maps.singleton (var "v") (Lists.head $ var "apptypes")) @@
                  (var "t2"))
                @@ (Lists.tail $ var "apptypes")])) $
      "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "term" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t1") $
      var "app" @@ var "t1" @@ var "apptypes")) $

  trace (Strings.cat $ list [
    string "checking type of: ",
    ref ShowCore.termDef @@ var "term",
    string " (vars: ",
    ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Sets.toList $ var "vars"),
    string ", apptypes: ",
    ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "apptypes",
    string ", types: ",
    ref Formatting.showListDef @@ unaryFunction Core.unName @@ (Maps.keys $ var "types"),
    string ")"]) $

  cases _Term (var "term")
    (Just $ Flows.fail $ Strings.cat $ list [
      "unsupported term variant in typeOf: ",
      ref ShowMantle.termVariantDef @@ (ref Variants.termVariantDef @@ var "term")]) [

    _Term_annotated>>: "at" ~> var "checkApp" @@ (
      "term1" <~ Core.annotatedTermSubject (var "at") $
      ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ var "apptypes" @@ var "term1"),

    _Term_application>>: "app" ~> var "checkApp" @@ (
      "a" <~ Core.applicationFunction (var "app") $
      "b" <~ Core.applicationArgument (var "app") $
      "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "a" $
      "t2" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "b" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t1") $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t2") $
      "tryType" <~ ("t" ~> cases _Type (var "t")
        (Just $ Flows.fail $ Strings.cat $ list [
          "left hand side of application ",
          ref ShowCore.termDef @@ var "term",
          " is not a function type: ",
          ref ShowCore.typeDef @@ var "t"]) [
        -- These forall types can arise from bindUnboundTypeVariables
        _Type_forall>>: "ft" ~> var "tryType" @@ (Core.forallTypeBody (var "ft")),
        _Type_function>>: "ft" ~>
          "p" <~ Core.functionTypeDomain (var "ft") $
          "q" <~ Core.functionTypeCodomain (var "ft") $
          Logic.ifElse (Equality.equal (var "p") (var "t2"))
            (Flows.pure $ var "q")
            (Flows.fail $ Strings.cat $ list [
              "expected ",
              ref ShowCore.typeDef @@ var "p",
              " in ",
              ref ShowCore.termDef @@ var "term",
              " but found ",
              ref ShowCore.typeDef @@ var "t2"])]) $
        var "tryType" @@ var "t1"),

    _Term_function>>: "f" ~>
      cases _Function (var "f") Nothing [
        _Function_elimination>>: "elm" ~>
          cases _Elimination (var "elm") Nothing [
            _Elimination_product>>: "tp" ~> var "checkApp" @@ (
              "index" <~ Core.tupleProjectionIndex (var "tp") $
              "arity" <~ Core.tupleProjectionArity (var "tp") $
              "mtypes" <~ Core.tupleProjectionDomain (var "tp") $
              Optionals.maybe
                (Flows.fail $ Strings.cat $ list [
                  "untyped tuple projection: ",
                  ref ShowCore.termDef @@ var "term"])
                ( "types" ~>
                  exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "types")) $
                  Flows.pure $ Core.typeFunction $ Core.functionType
                    (Core.typeProduct $ var "types")
                    (Lists.at (var "index") (var "types")))
                (var "mtypes")),

            _Elimination_record>>: "p" ~>
              "tname" <~ Core.projectionTypeName (var "p") $
              "fname" <~ Core.projectionField (var "p") $
              "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
              "svars" <~ Core.typeSchemeVariables (var "schemaType") $
              "stype" <~ Core.typeSchemeType (var "schemaType") $
              "sfields" <<~ ref ExtractCore.recordTypeDef @@ var "tname" @@ var "stype" $
              "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $
              "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "apptypes")) $
              "sftyp" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "ftyp" $
              Flows.pure $ Core.typeFunction $ Core.functionType
                (ref Schemas.nominalApplicationDef @@ var "tname"  @@ var "apptypes")
                (var "sftyp"),

            _Elimination_union>>: "cs" ~>
              "tname" <~ Core.caseStatementTypeName (var "cs") $
              "dflt" <~ Core.caseStatementDefault (var "cs") $
              "cases" <~ Core.caseStatementCases (var "cs") $
              "cterms" <~ Lists.map (unaryFunction Core.fieldTerm) (var "cases") $
              "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
              "svars" <~ Core.typeSchemeVariables (var "schemaType") $
              "stype" <~ Core.typeSchemeType (var "schemaType") $
              "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
              "tdflt" <<~ Flows.mapOptional ("e" ~> ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "e") (var "dflt") $
              "tcterms" <<~ Flows.mapList ("e" ~> ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "e") (var "cterms") $
              "cods" <<~ Flows.mapList ("t" ~> Flows.map (unaryFunction Core.functionTypeCodomain) $ ref ExtractCore.functionTypeDef @@ var "t") (var "tcterms") $
              "ts" <~ Optionals.cat (Lists.cons (var "tdflt") $ Lists.map (unaryFunction Optionals.pure) (var "cods")) $
              "cod" <<~ ref checkSameTypeDef @@ string "case branches" @@ var "ts" $
              "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "apptypes")) $
              "scod" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "cod" $
              Flows.pure $ Core.typeFunction $ Core.functionType
                (ref Schemas.nominalApplicationDef @@ var "tname"  @@ var "apptypes")
                (var "scod"),

            _Elimination_wrap>>: "tname" ~>
              "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
              "svars" <~ Core.typeSchemeVariables (var "schemaType") $
              "stype" <~ Core.typeSchemeType (var "schemaType") $
              "wrapped" <<~ ref ExtractCore.wrappedTypeDef @@ var "tname" @@ var "stype" $
              "subst" <~ Typing.typeSubst (Maps.fromList $ Lists.zip (var "svars") (var "apptypes")) $
              "swrapped" <~ ref Substitution.substInTypeDef @@ var "subst" @@ var "wrapped" $
              produce $ TTypes.function
                (ref Schemas.nominalApplicationDef @@ var "tname" @@ var "apptypes")
                (var "swrapped")],

        _Function_lambda>>: "l" ~> var "checkApp" @@ (
          "x" <~ Core.lambdaParameter (var "l") $
          "mt" <~ Core.lambdaDomain (var "l") $
          "e" <~ Core.lambdaBody (var "l") $
          Optionals.maybe
            (Flows.fail $ Strings.cat $ list [
              string "untyped lambda: ",
              ref ShowCore.termDef @@ var "term"])
            ("t" ~>
              exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t") $
              "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@
                (Maps.insert (var "x") (var "t") (var "types")) @@ list [] @@ var "e" $
              exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "t1") $
              Flows.pure $ Core.typeFunction $ Core.functionType (var "t") (var "t1"))
            (var "mt")),
        _Function_primitive>>: "name" ~> var "checkApp" @@ (
          -- Note: no instantiation
          "ts" <~ Optionals.maybe
            (Flows.fail $ Strings.cat $ list [
              string "no such primitive: ",
              Core.unName $ var "name"])
            (unaryFunction Flows.pure)
            (Maps.lookup (var "name") (Typing.inferenceContextPrimitiveTypes $ var "cx")) $
          Flows.map (ref Schemas.typeSchemeToFTypeDef) (var "ts"))],

    _Term_let>>: "letTerm" ~> var "checkApp" @@ (
      "bs" <~ Core.letBindings (var "letTerm") $
      "body" <~ Core.letEnvironment (var "letTerm") $
      "bnames" <~ Lists.map (unaryFunction Core.bindingName) (var "bs") $
      "bterms" <~ Lists.map (unaryFunction Core.bindingTerm) (var "bs") $
      "btypeOf" <~ ("b" ~>
        Optionals.maybe
          (Flows.fail $ Strings.cat $ list [
            string "untyped let binding in ",
            ref ShowCore.termDef @@ var "term"])
          ("ts" ~> Flows.pure $ ref Schemas.typeSchemeToFTypeDef @@ var "ts")
          (Core.bindingType $ var "b")) $
      "btypes" <<~ Flows.mapList (var "btypeOf") (var "bs") $
      "types2" <~ Maps.union (Maps.fromList $ Lists.zip (var "bnames") (var "btypes")) (var "types") $
      "typeofs" <<~ Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types2" @@ list []) (var "bterms") $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "btypes")) $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "typeofs")) $
      Logic.ifElse (Equality.equal (var "typeofs") (var "btypes"))
        (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types2" @@ list [] @@ var "body")
        (Flows.fail $ Strings.cat $ list [
          string "binding types disagree: ",
          ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "btypes",
          string " and ",
          ref Formatting.showListDef @@ ref ShowCore.typeDef @@ var "typeofs",
          string " from terms: ",
          ref Formatting.showListDef @@ ref ShowCore.termDef @@ var "bterms"])),

    _Term_list>>: "els" ~>
      Logic.ifElse (Lists.null $ var "els")
        -- Empty list is polymorphic
        (Logic.ifElse (Equality.equal (Lists.length $ var "apptypes") (int32 1))
          (Flows.pure $ Core.typeList $ Lists.head $ var "apptypes")
          (Flows.fail $ "list type applied to more or less than one argument"))
        -- Nonempty list must have elements of the same type
        ( "eltypes" <<~ Flows.mapList
           (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [])
           (var "els") $
           "unifiedType" <<~ ref checkSameTypeDef @@ string "list elements" @@ var "eltypes" $
           -- Verify the unified type is well-formed in the current scope
           exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "unifiedType") $
           Flows.pure $ Core.typeList $ var "unifiedType"),

    _Term_literal>>: "lit" ~> var "checkApp" @@ (
      Flows.pure $ Core.typeLiteral $ ref Variants.literalTypeDef @@ var "lit"),

    _Term_map>>: "m" ~>
      Logic.ifElse (Maps.null $ var "m")
        -- Empty map is polymorphic
        (Logic.ifElse (Equality.equal (Lists.length $ var "apptypes") (int32 2))
          (Flows.pure $ Core.typeMap $ Core.mapType
            (Lists.at (int32 1) $ var "apptypes")
            (Lists.at (int32 0) $ var "apptypes"))
          (Flows.fail $ "map type applied to more or less than two arguments"))
        -- Nonempty map must have keys and values of the same type
        (var "checkApp" @@ (
          "pairs" <~ Maps.toList (var "m") $
          "kt" <<~ Flows.bind
            (Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list []) $
              Lists.map (unaryFunction first) (var "pairs"))
            (ref checkSameTypeDef @@ string "map keys") $
          "vt" <<~ Flows.bind
            (Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list []) $
              Lists.map (unaryFunction second) (var "pairs"))
            (ref checkSameTypeDef @@ string "map values") $
          exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "kt") $
          exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "vt") $
          Flows.pure $ Core.typeMap $ Core.mapType (var "kt") (var "vt"))),

    _Term_optional>>: "mt" ~>
      Optionals.maybe
        -- Nothing case is polymorphic
        ("n" <~ Lists.length (var "apptypes") $
          Logic.ifElse (Equality.equal (var "n") (int32 1))
            (Flows.pure $ Core.typeOptional $ Lists.head $ var "apptypes")
            (Flows.fail $ "optional type applied to " ++ Literals.showInt32 (var "n") ++ " argument(s). Expected 1."))
        -- Just case: infer type of the contained term
        ("term" ~> var "checkApp" @@ (
          "termType" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "term" $
           exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "termType") $
           Flows.pure $ Core.typeOptional $ var "termType"))
        (var "mt"),

    _Term_product>>: "tuple" ~> var "checkApp" @@ (
      "etypes" <<~ Flows.mapList (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list []) (var "tuple") $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "etypes")) $
      Flows.pure $ Core.typeProduct $ var "etypes"),

    _Term_record>>: "record" ~>
      "tname" <~ Core.recordTypeName (var "record") $
      "fields" <~ Core.recordFields (var "record") $
      "ftypes" <<~ Flows.mapList
        (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [])
        (Lists.map (unaryFunction Core.fieldTerm) (var "fields")) $
      exec (Flows.mapList (ref checkTypeVariablesDef @@ var "cx" @@ var "vars") (var "ftypes")) $
      ref typeOfNominalDef
        @@ string "record typeOf"
        @@ var "cx"
        @@ var "tname"
        @@ (Core.typeRecord $ Core.rowType (var "tname") $ Lists.zipWith
          ("n" ~>
           "t" ~>
           Core.fieldType (var "n") (var "t"))
          (Lists.map (unaryFunction Core.fieldName) (var "fields"))
          (var "ftypes")),

    _Term_set>>: "els" ~>
      Logic.ifElse (Sets.null $ var "els")
        -- Empty set is polymorphic
        (Logic.ifElse (Equality.equal (Lists.length $ var "apptypes") (int32 1))
          (Flows.pure $ Core.typeSet $ Lists.head $ var "apptypes")
          (Flows.fail $ "set type applied to more or less than one argument"))
        -- Nonempty set must have elements of the same type
        ( "eltypes" <<~ Flows.mapList
           (ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [])
           (Sets.toList $ var "els") $
           "unifiedType" <<~ ref checkSameTypeDef @@ string "set elements" @@ var "eltypes" $
           -- Verify the unified type is well-formed in the current scope
           exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "unifiedType") $
           Flows.pure $ Core.typeSet $ var "unifiedType"),

    -- TermSum (Sum idx size term1) is ignored for now. See https://github.com/CategoricalData/hydra/issues/134.

    _Term_typeLambda>>: "ta" ~>
      "v" <~ Core.typeLambdaParameter (var "ta") $
      "e" <~ Core.typeLambdaBody (var "ta") $
      "t1" <<~ ref typeOfInternalDef @@ var "cx" @@ (Sets.insert (var "v") (var "vars")) @@ var "types" @@ list [] @@ var "e" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ (Sets.insert (var "v") (var "vars")) @@ var "t1") $
      Flows.pure $ Core.typeForall $ Core.forallType (var "v") (var "t1"),

    _Term_typeApplication>>: "tyapp" ~>
      "e" <~ Core.typedTermTerm (var "tyapp") $
      "t" <~ Core.typedTermType (var "tyapp") $
      ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ Lists.cons (var "t") (var "apptypes") @@ var "e",

    _Term_union>>: "injection" ~>
      "tname" <~ Core.injectionTypeName (var "injection") $

      -- Note: the following are only for sanity checking, and are not used in the typeOf result.
      "field" <~ Core.injectionField (var "injection") $
      "fname" <~ Core.fieldName (var "field") $
      "fterm" <~ Core.fieldTerm (var "field") $
      "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
      "svars" <~ Core.typeSchemeVariables (var "schemaType") $
      "stype" <~ Core.typeSchemeType (var "schemaType") $
      "sfields" <<~ ref ExtractCore.unionTypeDef @@ var "tname" @@ var "stype" $
      "ftyp" <<~ ref Schemas.findFieldTypeDef @@ var "fname" @@ var "sfields" $

      Flows.pure $ ref Schemas.nominalApplicationDef @@ var "tname"  @@ var "apptypes",

    _Term_unit>>: constant $ var "checkApp" @@ (Flows.pure Core.typeUnit),

    _Term_variable>>: "name" ~> var "checkApp" @@ (Optionals.maybe
      (Flows.fail $ Strings.cat $ list [
        string "unbound variable: ",
        Core.unName $ var "name"])
      (unaryFunction Flows.pure)
      (Maps.lookup (var "name") (var "types"))),

    _Term_wrap>>: "wt" ~>
      "tname" <~ Core.wrappedTermTypeName (var "wt") $
      "innerTerm" <~ Core.wrappedTermObject (var "wt") $
      "innerType" <<~ ref typeOfInternalDef @@ var "cx" @@ var "vars" @@ var "types" @@ list [] @@ var "innerTerm" $
      exec (ref checkTypeVariablesDef @@ var "cx" @@ var "vars" @@ var "innerType") $
      ref typeOfNominalDef @@ string "wrapper typeOf" @@ var "cx" @@ var "tname" @@
        (Core.typeWrap $ Core.wrappedType (var "tname") (var "innerType"))]

typeOfNominalDef :: TBinding (String -> InferenceContext -> Name -> Type -> Flow s Type)
typeOfNominalDef = define "typeOfNominal" $
  doc "Infer the type of a nominal type" $
  "desc" ~> "cx" ~> "tname" ~> "expected" ~>
  "resolveType" <~ ("subst" ~> "v" ~>
    Optionals.fromMaybe (Core.typeVariable $ var "v") (Maps.lookup (var "v") (var "subst"))) $
  "schemaType" <<~ ref Schemas.requireSchemaTypeDef @@ var "cx" @@ var "tname" $
  "svars" <~ Core.typeSchemeVariables (var "schemaType") $
  "stype" <~ Core.typeSchemeType (var "schemaType") $
  "substWrapper" <<~ ref Unification.unifyTypesDef @@ -- TODO: warning: unification within type checking
    (Typing.inferenceContextSchemaTypes $ var "cx") @@
    var "stype" @@
    var "expected" @@
    var "desc" $
  exec (ref checkTypeSubstDef @@ var "cx" @@ var "substWrapper") $
  "subst" <~ Typing.unTypeSubst (var "substWrapper") $
  "tparams" <~ Lists.map (var "resolveType" @@ var "subst") (var "svars") $
  Flows.pure $ ref Schemas.nominalApplicationDef @@ var "tname" @@ var "tparams"
