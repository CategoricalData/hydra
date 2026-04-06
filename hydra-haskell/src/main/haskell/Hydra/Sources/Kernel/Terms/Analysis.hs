
module Hydra.Sources.Kernel.Terms.Analysis where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  addNamesToNamespaces,
  analyzeFunctionTerm,
  analyzeFunctionTermWith,
  analyzeFunctionTermWith_finish,
  analyzeFunctionTermWith_gather,
  definitionDependencyNamespaces,
  dependencyNamespaces,
  gatherApplications,
  gatherArgs,
  gatherArgsWithTypeApps,
  isSimpleAssignment,
  isSelfTailRecursive,
  isTailRecursiveInTailPosition,
  moduleContainsBinaryLiterals,
  moduleDependencyNamespaces,
  namespacesForDefinitions)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Annotations  as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity        as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking     as Checking
import qualified Hydra.Sources.Kernel.Terms.Dependencies  as Dependencies
import qualified Hydra.Sources.Decode.Core  as DecodeCore
import qualified Hydra.Sources.Kernel.Terms.Lexical      as Lexical
import qualified Hydra.Sources.Kernel.Terms.Names        as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting    as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Scoping      as Scoping
import qualified Hydra.Sources.Kernel.Terms.Strip        as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables    as Variables

import qualified Hydra.Sources.Kernel.Terms.Predicates   as Predicates


ns :: Namespace
ns = Namespace "hydra.analysis"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns definitions
    [Annotations.ns, Arity.ns, Checking.ns, Dependencies.ns, moduleNamespace DecodeCore.module_, Lexical.ns, Names.ns, Predicates.ns, Rewriting.ns, Scoping.ns, Strip.ns, Variables.ns]
    kernelTypesNamespaces $
    Just ("Module dependency namespace analysis")
  where
    definitions = [
      toDefinition addNamesToNamespaces,
      toDefinition analyzeFunctionTerm,
      toDefinition analyzeFunctionTermWith,
      toDefinition analyzeFunctionTermWith_finish,
      toDefinition analyzeFunctionTermWith_gather,
      toDefinition definitionDependencyNamespaces,
      toDefinition dependencyNamespaces,
      toDefinition gatherApplications,
      toDefinition gatherArgs,
      toDefinition gatherArgsWithTypeApps,
      toDefinition isSimpleAssignment,
      toDefinition isSelfTailRecursive,
      toDefinition isTailRecursiveInTailPosition,
      toDefinition moduleContainsBinaryLiterals,
      toDefinition moduleDependencyNamespaces,
      toDefinition namespacesForDefinitions]

addNamesToNamespaces :: TTermDefinition ((Namespace -> a) -> S.Set Name -> Namespaces a -> Namespaces a)
addNamesToNamespaces = define "addNamesToNamespaces" $
  doc "Add names to existing namespaces mapping" $
  "encodeNamespace" ~> "names" ~> "ns0" ~>
--  "nss" <~ Sets.empty $
  "nss" <~ Sets.fromList (Maybes.cat $ Lists.map (Names.namespaceOf) $ Sets.toList $ var "names") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Packaging.namespacesWithMapping (var "ns0") $ Maps.union
    (Packaging.namespacesMapping $ var "ns0")
    (Maps.fromList $ Lists.map (var "toPair") $ Sets.toList $ var "nss")

analyzeFunctionTerm :: TTermDefinition (
  Context ->
  (env -> Graph) ->
  (Graph -> env -> env) ->
  env ->
  Term ->
  Either (InContext Error) (FunctionStructure env))
analyzeFunctionTerm = define "analyzeFunctionTerm" $
  doc "Analyze a function term, collecting lambdas, type lambdas, lets, and type applications" $
  "cx" ~> "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermWith @@ var "cx"
    @@ ("g" ~> "b" ~> Logic.ifElse (Predicates.isComplexBinding @@ var "g" @@ var "b") (just MetaTerms.true) nothing)
    @@ var "getTC" @@ var "setTC" @@ var "env" @@ var "term"

analyzeFunctionTermWith :: TTermDefinition (
  Context ->
  (Graph -> Binding -> Maybe Term) ->
  (env -> Graph) ->
  (Graph -> env -> env) ->
  env ->
  Term ->
  Either (InContext Error) (FunctionStructure env))
analyzeFunctionTermWith = define "analyzeFunctionTermWith" $
  doc "Analyze a function term with configurable binding metadata" $
  "cx" ~> "forBinding" ~> "getTC" ~> "setTC" ~> "env" ~> "term" ~>
  analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
    @@ boolean True @@ var "env"
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Name])
    @@ list ([] :: [TTerm Binding])
    @@ list ([] :: [TTerm Type])
    @@ list ([] :: [TTerm Type])
    @@ var "term"

analyzeFunctionTermWith_finish :: TTermDefinition (
  Context ->
  (env -> Graph) ->
  env -> [Name] -> [Name] -> [Binding] -> [Type] -> [Type] -> Term ->
  Either (InContext Error) (FunctionStructure env))
analyzeFunctionTermWith_finish = define "analyzeFunctionTermWith_finish" $
  "cx" ~> "getTC" ~> "fEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "body" ~>
  "bodyWithTapps" <~ Lists.foldl
    ("trm" ~> "typ" ~> Core.termTypeApplication (Core.typeApplicationTerm (var "trm") (var "typ")))
    (var "body")
    (var "tapps") $
  -- Use typeOfTerm but fall back to Nothing if type inference fails (e.g. for untyped hoisted bindings)
  "mcod" <~ Eithers.either_ (constant nothing) ("c" ~> just (var "c"))
    (Checking.typeOfTerm @@ var "cx" @@ (var "getTC" @@ var "fEnv") @@ var "bodyWithTapps") $
  right $ record _FunctionStructure [
    _FunctionStructure_typeParams>>: Lists.reverse (var "tparams"),
    _FunctionStructure_params>>: Lists.reverse (var "args"),
    _FunctionStructure_bindings>>: var "bindings",
    _FunctionStructure_body>>: var "bodyWithTapps",
    _FunctionStructure_domains>>: Lists.reverse (var "doms"),
    _FunctionStructure_codomain>>: var "mcod",
    _FunctionStructure_environment>>: var "fEnv"]

analyzeFunctionTermWith_gather :: TTermDefinition (
  Context ->
  (Graph -> Binding -> Maybe Term) ->
  (env -> Graph) ->
  (Graph -> env -> env) ->
  Bool -> env -> [Name] -> [Name] -> [Binding] -> [Type] -> [Type] -> Term ->
  Either (InContext Error) (FunctionStructure env))
analyzeFunctionTermWith_gather = define "analyzeFunctionTermWith_gather" $
  "cx" ~> "forBinding" ~> "getTC" ~> "setTC" ~>
  "argMode" ~> "gEnv" ~> "tparams" ~> "args" ~> "bindings" ~> "doms" ~> "tapps" ~> "t" ~>
  cases _Term (Strip.deannotateTerm @@ var "t")
    (Just $ analyzeFunctionTermWith_finish @@ var "cx" @@ var "getTC" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ analyzeFunctionTermWith_finish @@ var "cx" @@ var "getTC" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t") [
        _Function_lambda>>: "lam" ~>
          Logic.ifElse (var "argMode")
            ("v" <~ Core.lambdaParameter (var "lam") $
             "dom" <~ Maybes.maybe (Core.typeVariable (Core.name (string "_"))) identity (Core.lambdaDomain (var "lam")) $
             "body" <~ Core.lambdaBody (var "lam") $
             "newEnv" <~ (var "setTC" @@ (Scoping.extendGraphForLambda @@ (var "getTC" @@ var "gEnv") @@ var "lam") @@ var "gEnv") $
             analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
               @@ var "argMode" @@ var "newEnv"
               @@ var "tparams"
               @@ (Lists.cons (var "v") (var "args"))
               @@ var "bindings"
               @@ (Lists.cons (var "dom") (var "doms"))
               @@ var "tapps"
               @@ var "body")
            (analyzeFunctionTermWith_finish @@ var "cx" @@ var "getTC" @@ var "gEnv" @@ var "tparams" @@ var "args" @@ var "bindings" @@ var "doms" @@ var "tapps" @@ var "t")],
    _Term_let>>: "lt" ~>
      "newBindings" <~ Core.letBindings (var "lt") $
      "body" <~ Core.letBody (var "lt") $
      "newEnv" <~ (var "setTC" @@ (Scoping.extendGraphForLet @@ var "forBinding" @@ (var "getTC" @@ var "gEnv") @@ var "lt") @@ var "gEnv") $
      analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
        @@ boolean False @@ var "newEnv"
        @@ var "tparams"
        @@ var "args"
        @@ (Lists.concat2 (var "bindings") (var "newBindings"))
        @@ var "doms"
        @@ var "tapps"
        @@ var "body",
    _Term_typeApplication>>: "ta" ~>
      "taBody" <~ Core.typeApplicationTermBody (var "ta") $
      "typ" <~ Core.typeApplicationTermType (var "ta") $
      analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
        @@ var "argMode" @@ var "gEnv"
        @@ var "tparams"
        @@ var "args"
        @@ var "bindings"
        @@ var "doms"
        @@ (Lists.cons (var "typ") (var "tapps"))
        @@ var "taBody",
    _Term_typeLambda>>: "tl" ~>
      "tvar" <~ Core.typeLambdaParameter (var "tl") $
      "tlBody" <~ Core.typeLambdaBody (var "tl") $
      "newEnv" <~ (var "setTC" @@ (Scoping.extendGraphForTypeLambda @@ (var "getTC" @@ var "gEnv") @@ var "tl") @@ var "gEnv") $
      analyzeFunctionTermWith_gather @@ var "cx" @@ var "forBinding" @@ var "getTC" @@ var "setTC"
        @@ var "argMode" @@ var "newEnv"
        @@ (Lists.cons (var "tvar") (var "tparams"))
        @@ var "args"
        @@ var "bindings"
        @@ var "doms"
        @@ var "tapps"
        @@ var "tlBody"]

definitionDependencyNamespaces :: TTermDefinition ([Definition] -> S.Set Namespace)
definitionDependencyNamespaces = define "definitionDependencyNamespaces" $
  doc "Get dependency namespaces from definitions" $
  "defs" ~>
  "defNames" <~ ("def" ~> cases _Definition (var "def")
    Nothing [
    _Definition_type>>: "typeDef" ~>
      Dependencies.typeDependencyNames @@ true @@ (Core.typeSchemeType $ Packaging.typeDefinitionType (var "typeDef")),
    _Definition_term>>: "termDef" ~>
      Dependencies.termDependencyNames @@ true @@ true @@ true @@ Packaging.termDefinitionTerm (var "termDef")]) $
  "allNames" <~ Sets.unions (Lists.map (var "defNames") (var "defs")) $
  Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (Sets.toList (var "allNames"))))

dependencyNamespaces :: TTermDefinition (Context -> Graph -> Bool -> Bool -> Bool -> Bool -> [Binding] -> Either (InContext Error) (S.Set Namespace))
dependencyNamespaces = define "dependencyNamespaces" $
  doc "Find dependency namespaces in all of a set of terms (Either version)" $
  "cx" ~> "graph" ~> "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "els" ~>
  "depNames" <~ ("el" ~>
    "term" <~ Core.bindingTerm (var "el") $
    "deannotatedTerm" <~ Strip.deannotateTerm @@ var "term" $
    "dataNames" <~ Dependencies.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "term" $
    "schemaNames" <~ Logic.ifElse (var "withSchema")
      (Maybes.maybe Sets.empty
        ("ts" ~> Dependencies.typeDependencyNames @@ true @@ Core.typeSchemeType (var "ts"))
        (Core.bindingType (var "el")))
      Sets.empty $
    -- Handle encoded types: decode as Type and extract type dependency names
    Logic.ifElse (Predicates.isEncodedType @@ var "deannotatedTerm")
      (Eithers.map ("typ" ~> Sets.unions (list [
          var "dataNames", var "schemaNames",
          Dependencies.typeDependencyNames @@ true @@ var "typ"]))
        (Ctx.withContext (Ctx.pushTrace (string "dependency namespace (type)") (var "cx"))
          (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
            (decoderFor _Type @@ var "graph" @@ var "term"))))
      -- Handle encoded terms: decode as Term and extract term dependency names
      (Logic.ifElse (Predicates.isEncodedTerm @@ var "deannotatedTerm")
        (Eithers.map ("decodedTerm" ~> Sets.unions (list [
            var "dataNames", var "schemaNames",
            Dependencies.termDependencyNames @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "decodedTerm"]))
          (Ctx.withContext (Ctx.pushTrace (string "dependency namespace (term)") (var "cx"))
            (Eithers.bimap ("_e" ~> Error.errorOther $ Error.otherError (unwrap _DecodingError @@ var "_e")) ("_a" ~> var "_a")
              (decoderFor _Term @@ var "graph" @@ var "term"))))
        (right (Sets.unions (list [var "dataNames", var "schemaNames"]))))) $
  Eithers.map ("namesList" ~> Sets.fromList (Maybes.cat (Lists.map (Names.namespaceOf) (
      Sets.toList (Sets.unions (var "namesList"))))))
    (Eithers.mapList (var "depNames") (var "els"))

gatherApplications :: TTermDefinition (Term -> ([Term], Term))
gatherApplications = define "gatherApplications" $
  doc "Gather applications from a term, returning (args, baseTerm)" $
  "term" ~>
  -- Use a local recursive helper with an accumulator
  "go" <~ ("args" ~> "t" ~>
    cases _Term (Strip.deannotateTerm @@ var "t")
      (Just $ pair (var "args") (var "t")) [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        var "go" @@ (Lists.cons (var "rhs") (var "args")) @@ var "lhs"]) $
  var "go" @@ (list ([] :: [TTerm Term])) @@ var "term"

gatherArgs :: TTermDefinition (Term -> [Term] -> (Term, [Term]))
gatherArgs = define "gatherArgs" $
  doc "Gather term arguments, stripping type-level constructs" $
  "term" ~> "args" ~>
  cases _Term (Strip.deannotateTerm @@ var "term")
    (Just $ pair (var "term") (var "args")) [
    _Term_application>>: "app" ~>
      "lhs" <~ Core.applicationFunction (var "app") $
      "rhs" <~ Core.applicationArgument (var "app") $
      gatherArgs @@ var "lhs" @@ (Lists.cons (var "rhs") (var "args")),
    _Term_typeLambda>>: "tl" ~>
      "body" <~ Core.typeLambdaBody (var "tl") $
      gatherArgs @@ var "body" @@ var "args",
    _Term_typeApplication>>: "ta" ~>
      "body" <~ Core.typeApplicationTermBody (var "ta") $
      gatherArgs @@ var "body" @@ var "args"]

gatherArgsWithTypeApps :: TTermDefinition (Term -> [Term] -> [Type] -> (Term, [Term], [Type]))
gatherArgsWithTypeApps = define "gatherArgsWithTypeApps" $
  doc "Gather term and type arguments from a term" $
  "term" ~> "args" ~> "tyArgs" ~>
  cases _Term (Strip.deannotateTerm @@ var "term")
    (Just $ triple (var "term") (var "args") (var "tyArgs")) [
    _Term_application>>: "app" ~>
      "lhs" <~ Core.applicationFunction (var "app") $
      "rhs" <~ Core.applicationArgument (var "app") $
      gatherArgsWithTypeApps @@ var "lhs" @@ (Lists.cons (var "rhs") (var "args")) @@ var "tyArgs",
    _Term_typeLambda>>: "tl" ~>
      "body" <~ Core.typeLambdaBody (var "tl") $
      gatherArgsWithTypeApps @@ var "body" @@ var "args" @@ var "tyArgs",
    _Term_typeApplication>>: "ta" ~>
      "body" <~ Core.typeApplicationTermBody (var "ta") $
      "typ" <~ Core.typeApplicationTermType (var "ta") $
      gatherArgsWithTypeApps @@ var "body" @@ var "args" @@ (Lists.cons (var "typ") (var "tyArgs"))]

isSelfTailRecursive :: TTermDefinition (Name -> Term -> Bool)
isSelfTailRecursive = define "isSelfTailRecursive" $
  doc "Check if a term body is self-tail-recursive with respect to a function name" $
  "funcName" ~> "body" ~>
    -- isFreeVariableInTerm returns True when v is NOT free (not present).
    -- So Logic.not means: the name IS present as a free variable.
    "callsSelf" <~ Logic.not (Variables.isFreeVariableInTerm @@ var "funcName" @@ var "body") $
    Logic.ifElse (var "callsSelf")
      (isTailRecursiveInTailPosition @@ var "funcName" @@ var "body")
      false

isSimpleAssignment :: TTermDefinition (Term -> Bool)
isSimpleAssignment = define "isSimpleAssignment" $
  doc "Check if a term can be encoded as a simple assignment" $
  "term" ~>
  cases _Term (var "term")
    (Just $
      -- Check if the base term (after gathering args) is a union elimination
      "baseTerm" <~ Pairs.first (gatherArgs @@ var "term" @@ list ([] :: [TTerm Term])) $
      cases _Term (var "baseTerm")
        (Just $ boolean True) [
        _Term_function>>: "f" ~>
          cases _Function (var "f")
            (Just $ boolean True) [
            _Function_elimination>>: "elim" ~>
              cases _Elimination (var "elim")
                (Just $ boolean True) [
                _Elimination_union>>: constant (boolean False)]]]) [
    _Term_annotated>>: "at" ~>
      isSimpleAssignment @@ (Core.annotatedTermBody $ var "at"),
    _Term_function>>: "f" ~>
      cases _Function (var "f")
        (Just $ boolean True) [
        _Function_lambda>>: constant (boolean False)],
    _Term_let>>: constant (boolean False),
    _Term_typeLambda>>: constant (boolean False),
    _Term_typeApplication>>: "ta" ~>
      isSimpleAssignment @@ (Core.typeApplicationTermBody $ var "ta")]

isTailRecursiveInTailPosition :: TTermDefinition (Name -> Term -> Bool)
isTailRecursiveInTailPosition = define "isTailRecursiveInTailPosition" $
  doc "Check that all self-references are in tail position" $
  "funcName" ~> "term" ~>
    "stripped" <~ (Strip.deannotateAndDetypeTerm @@ var "term") $
    cases _Term (var "stripped") (Just $
      -- Default: funcName must NOT appear free in this term (not a recognized tail position)
      Variables.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
      -- Application: check if it's a self-tail-call or a case statement application
      _Term_application>>: "app" ~>
        "gathered" <~ (gatherApplications @@ var "stripped") $
        "gatherArgs" <~ (Pairs.first $ var "gathered") $
        "gatherFun" <~ (Pairs.second $ var "gathered") $
        "strippedFun" <~ (Strip.deannotateAndDetypeTerm @@ var "gatherFun") $
        cases _Term (var "strippedFun") (Just $
          -- Unknown function form: funcName must not appear anywhere
          Variables.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
          -- Variable: check if self-call
          _Term_variable>>: "vname" ~>
            Logic.ifElse (Equality.equal (var "vname") (var "funcName"))
              -- Self-call in tail position: args must not contain funcName
              -- and must not contain lambdas (closures over parameters break TCO
              -- because Python closures capture by reference, not by value)
              ("argsNoFunc" <~ (Lists.foldl
                ("ok" ~> "arg" ~>
                  Logic.and (var "ok")
                    (Variables.isFreeVariableInTerm @@ var "funcName" @@ var "arg"))
                true
                (var "gatherArgs")) $
               "argsNoLambda" <~ (Lists.foldl
                ("ok" ~> "arg" ~>
                  Logic.and (var "ok")
                    (Logic.not $ Rewriting.foldOverTerm @@ Coders.traversalOrderPre
                      @@ ("found" ~> "t" ~>
                        Logic.or (var "found")
                          (cases _Term (var "t") (Just false) [
                            _Term_function>>: "f2" ~>
                              cases _Function (var "f2") (Just false) [
                                _Function_lambda>>: "lam" ~>
                                  -- Any lambda in an argument disqualifies from TCO
                                  "ignore" <~ (Core.lambdaBody $ var "lam") $
                                  true]]))
                      @@ false
                      @@ var "arg"))
                true
                (var "gatherArgs")) $
               Logic.and (var "argsNoFunc") (var "argsNoLambda"))
              -- Not a self-call: funcName must not appear anywhere in the term
              (Variables.isFreeVariableInTerm @@ var "funcName" @@ var "term"),
          -- Function: check for case statement (union elimination)
          _Term_function>>: "f" ~>
            cases _Function (var "f") (Just $
              Variables.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
              _Function_elimination>>: "e" ~>
                cases _Elimination (var "e") (Just $
                  Variables.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
                  _Elimination_union>>: "cs" ~>
                    "cases_" <~ (Core.caseStatementCases $ var "cs") $
                    "dflt" <~ (Core.caseStatementDefault $ var "cs") $
                    -- All case branches must have funcName only in tail position
                    "branchesOk" <~ (Lists.foldl
                      ("ok" ~> "field" ~>
                        Logic.and (var "ok")
                          (isTailRecursiveInTailPosition @@ var "funcName" @@ Core.fieldTerm (var "field")))
                      true
                      (var "cases_")) $
                    -- Default branch (if present) must also be tail-recursive
                    "dfltOk" <~ (Maybes.maybe true
                      ("d" ~> isTailRecursiveInTailPosition @@ var "funcName" @@ var "d")
                      (var "dflt")) $
                    -- Arguments to the case statement must NOT contain funcName
                    "argsOk" <~ (Lists.foldl
                      ("ok" ~> "arg" ~>
                        Logic.and (var "ok")
                          (Variables.isFreeVariableInTerm @@ var "funcName" @@ var "arg"))
                      true
                      (var "gatherArgs")) $
                    Logic.and (Logic.and (var "branchesOk") (var "dfltOk")) (var "argsOk")]]],
      -- Lambda: tail position is the body
      _Term_function>>: "f" ~>
        cases _Function (var "f") (Just $
          Variables.isFreeVariableInTerm @@ var "funcName" @@ var "term") [
          _Function_lambda>>: "lam" ~>
            isTailRecursiveInTailPosition @@ var "funcName" @@ (Core.lambdaBody $ var "lam")],
      -- Let: tail position is the body; bindings must not contain funcName
      _Term_let>>: "lt" ~>
        "bindingsOk" <~ (Lists.foldl
          ("ok" ~> "b" ~>
            Logic.and (var "ok")
              (Variables.isFreeVariableInTerm @@ var "funcName" @@ Core.bindingTerm (var "b")))
          true
          (Core.letBindings $ var "lt")) $
        Logic.and (var "bindingsOk")
          (isTailRecursiveInTailPosition @@ var "funcName" @@ (Core.letBody $ var "lt"))]

moduleContainsBinaryLiterals :: TTermDefinition (Module -> Bool)
moduleContainsBinaryLiterals = define "moduleContainsBinaryLiterals" $
  doc "Check whether a module contains any binary literal values" $
  "mod" ~>
  "checkTerm" <~ ("found" ~> "term" ~> Logic.or (var "found") $
    cases _Term (var "term") (Just false) [
      _Term_literal>>: "lit" ~>
        cases _Literal (var "lit") (Just false) [
          _Literal_binary>>: constant true]]) $
  "termContainsBinary" <~ ("term" ~>
    Rewriting.foldOverTerm @@ Coders.traversalOrderPre @@ var "checkTerm" @@ false @@ var "term") $
  "defTerms" <~ Maybes.cat (Lists.map
    ("d" ~> cases _Definition (var "d") (Just nothing) [
      _Definition_term>>: "td" ~> just (Packaging.termDefinitionTerm $ var "td")])
    (Packaging.moduleDefinitions (var "mod"))) $
  Lists.foldl
    ("acc" ~> "t" ~> Logic.or (var "acc") (var "termContainsBinary" @@ var "t"))
    false
    (var "defTerms")

moduleDependencyNamespaces :: TTermDefinition (Context -> Graph -> Bool -> Bool -> Bool -> Bool -> Module -> Either (InContext Error) (S.Set Namespace))
moduleDependencyNamespaces = define "moduleDependencyNamespaces" $
  doc "Find dependency namespaces in all elements of a module, excluding the module's own namespace (Either version)" $
  "cx" ~> "graph" ~> "binds" ~> "withPrims" ~> "withNoms" ~> "withSchema" ~> "mod" ~>
  "allBindings" <~ Maybes.cat (Lists.map
    ("d" ~> cases _Definition (var "d") (Just nothing) [
      _Definition_type>>: "td" ~>
        just (Annotations.typeBinding @@ (Packaging.typeDefinitionName $ var "td") @@ (Core.typeSchemeType $ Packaging.typeDefinitionType $ var "td")),
      _Definition_term>>: "td" ~>
        just (Core.binding (Packaging.termDefinitionName $ var "td") (Packaging.termDefinitionTerm $ var "td")
          (Packaging.termDefinitionType $ var "td"))])
    (Packaging.moduleDefinitions (var "mod"))) $
  Eithers.map
    ("deps" ~> Sets.delete (Packaging.moduleNamespace (var "mod")) (var "deps"))
    (dependencyNamespaces @@ var "cx" @@ var "graph" @@ var "binds" @@ var "withPrims" @@ var "withNoms" @@ var "withSchema" @@
      (var "allBindings"))

namespacesForDefinitions :: TTermDefinition ((Namespace -> a) -> Namespace -> [Definition] -> Namespaces a)
namespacesForDefinitions = define "namespacesForDefinitions" $
  doc "Create namespaces mapping for definitions" $
  "encodeNamespace" ~> "focusNs" ~> "defs" ~>
  "nss" <~ Sets.delete (var "focusNs") (definitionDependencyNamespaces @@ var "defs") $
  "toPair" <~ ("ns" ~> pair (var "ns") (var "encodeNamespace" @@ var "ns")) $
  Packaging.namespaces (var "toPair" @@ var "focusNs") (Maps.fromList (Lists.map (var "toPair") (Sets.toList (var "nss"))))

