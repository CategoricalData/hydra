module Hydra.Sources.Kernel.Terms.Dependencies where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  elementsWithDependencies,
  flattenLetTerms,
  inlineType,
  isLambda,
  liftLambdaAboveLet,
  pruneLet,
  replaceTypedefs,
  simplifyTerm,
  termDependencyNames,
  toShortNames,
  topologicalSortBindingMap,
  topologicalSortBindings,
  topologicalSortTypeDefinitions,
  typeDependencyNames,
  typeNamesInType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths        as Paths
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
import qualified Hydra.Dsl.Module       as Module
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
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Names as Names
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Kernel.Terms.Variables as Variables


ns :: Namespace
ns = Namespace "hydra.dependencies"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module ns elements
    [Lexical.ns, Names.ns, Rewriting.ns, Sorting.ns, Strip.ns, Variables.ns]
    kernelTypesNamespaces $
    Just ("Dependency extraction, binding sort, and let normalization")
  where
   elements = [
     toDefinition elementsWithDependencies,
     toDefinition flattenLetTerms,
     toDefinition inlineType,
     toDefinition isLambda,
     toDefinition liftLambdaAboveLet,
     toDefinition pruneLet,
     toDefinition replaceTypedefs,
     toDefinition simplifyTerm,
     toDefinition termDependencyNames,
     toDefinition toShortNames,
     toDefinition topologicalSortBindingMap,
     toDefinition topologicalSortBindings,
     toDefinition topologicalSortTypeDefinitions,
     toDefinition typeDependencyNames,
     toDefinition typeNamesInType]

flattenLetTerms :: TTermDefinition (Term -> Term)
flattenLetTerms = define "flattenLetTerms" $
  doc "Flatten nested let expressions" $
  "term" ~>
  "rewriteBinding" <~ ("binding" ~>
    "key0" <~ Core.bindingName (var "binding") $
    "val0" <~ Core.bindingTerm (var "binding") $
    "t" <~ Core.bindingType (var "binding") $
    cases _Term (var "val0")
      (Just $ pair (Core.binding (var "key0") (var "val0") (var "t")) (list ([] :: [TTerm Binding]))) [
      _Term_annotated>>: "at" ~>
        "val1" <~ Core.annotatedTermBody (var "at") $
        "ann" <~ Core.annotatedTermAnnotation (var "at") $
        "recursive" <~ var "rewriteBinding" @@ (Core.binding (var "key0") (var "val1") (var "t")) $
        "innerBinding" <~ Pairs.first (var "recursive") $
        "deps" <~ Pairs.second (var "recursive") $
        "val2" <~ Core.bindingTerm (var "innerBinding") $
        pair
          (Core.binding (var "key0") (Core.termAnnotated $ Core.annotatedTerm (var "val2") (var "ann")) (var "t"))
          (var "deps"),
      _Term_let>>: "innerLet" ~>
        "bindings1" <~ Core.letBindings (var "innerLet") $
        "body1" <~ Core.letBody (var "innerLet") $
        "prefix" <~ Strings.cat2 (unwrap _Name @@ var "key0") (string "_") $
        "qualify" <~ ("n" ~> Core.name $ Strings.cat2 (var "prefix") (unwrap _Name @@ var "n")) $
        "toSubstPair" <~ ("b" ~> pair (Core.bindingName $ var "b") (var "qualify" @@ (Core.bindingName $ var "b"))) $
        "subst" <~ Maps.fromList (Lists.map (var "toSubstPair") (var "bindings1")) $
        "replaceVars" <~ Variables.substituteVariables @@ var "subst" $
        "newBody" <~ var "replaceVars" @@ var "body1" $
        "newBinding" <~ ("b" ~> Core.binding
          (var "qualify" @@ (Core.bindingName $ var "b"))
          (var "replaceVars" @@ (Core.bindingTerm $ var "b"))
          (Core.bindingType $ var "b")) $
        pair
          (Core.binding (var "key0") (var "newBody") (var "t"))
          (Lists.map (var "newBinding") (var "bindings1"))]) $
  -- flattenBodyLet: if body is a let, merge its bindings into the outer let
  -- Note: The default case uses concat2 with empty list to force bindings to have type [Binding]
  -- This ensures proper type inference and prevents incorrect generalization
  "flattenBodyLet" <~ ("bindings" ~> "body" ~>
    cases _Term (var "body") (Just $ pair (Lists.concat2 (list ([] :: [TTerm Binding])) (var "bindings")) (var "body")) [
      _Term_let>>: "innerLt" ~>
        "innerBindings" <~ Core.letBindings (var "innerLt") $
        "innerBody" <~ Core.letBody (var "innerLt") $
        var "flattenBodyLet" @@ Lists.concat2 (var "bindings") (var "innerBindings") @@ var "innerBody"]) $
  "flatten" <~ ("recurse" ~> "term" ~>
    "rewritten" <~ var "recurse" @@ var "term" $
    cases _Term (var "rewritten")
      (Just $ var "rewritten") [
      _Term_let>>: "lt" ~>
        "bindings" <~ Core.letBindings (var "lt") $
        "body" <~ Core.letBody (var "lt") $
        -- Put dependencies BEFORE the binding that depends on them
        -- This is important for hoisting: dependencies need to be hoisted first
        -- so that transitive capture works correctly
        "forResult" <~ ("hr" ~> Lists.concat2 (Pairs.second $ var "hr") (Lists.pure (Pairs.first $ var "hr"))) $
        "flattenedBindings" <~ Lists.concat (Lists.map (var "forResult" <.> var "rewriteBinding") (var "bindings")) $
        -- Now check if body is also a let and merge those bindings too
        "merged" <~ var "flattenBodyLet" @@ var "flattenedBindings" @@ var "body" $
        "newBindings" <~ Pairs.first (var "merged") $
        "newBody" <~ Pairs.second (var "merged") $
        Core.termLet $ Core.let_ (var "newBindings") (var "newBody")]) $
  Rewriting.rewriteTerm @@ var "flatten" @@ var "term"

inlineType :: TTermDefinition (M.Map Name Type -> Type -> Prelude.Either String Type)
inlineType = define "inlineType" $
  doc "Inline all type variables in a type using the provided schema (Either version). Note: this function is only appropriate for nonrecursive type definitions" $
  "schema" ~> "typ" ~>
  "f" <~ ("recurse" ~> "typ" ~>
    "afterRecurse" <~ ("tr" ~> cases _Type (var "tr")
      (Just $ right $ var "tr") [
      _Type_variable>>: "v" ~>
        Maybes.maybe
          (left $ Strings.cat2 (string "No such type in schema: ") (unwrap _Name @@ var "v"))
          (inlineType @@ var "schema")
          (Maps.lookup (var "v") (var "schema"))]) $
    "tr" <<~ var "recurse" @@ var "typ" $
    var "afterRecurse" @@ var "tr") $
  Rewriting.rewriteTypeM @@ var "f" @@ var "typ"

isLambda :: TTermDefinition (Term -> Bool)
isLambda = define "isLambda" $
  doc "Check whether a term is a lambda, possibly nested within let and/or annotation terms" $
  "term" ~> cases _Term (Strip.deannotateTerm @@ var "term")
    (Just false) [
    _Term_function>>: match _Function
      (Just false) [
      _Function_lambda>>: constant true],
    _Term_let>>: "lt" ~> isLambda @@ (project _Let _Let_body @@ var "lt")]

-- TODO: account for shadowing among let- and lambda-bound variables
liftLambdaAboveLet :: TTermDefinition (Term -> Term)
liftLambdaAboveLet = define "liftLambdaAboveLet" $
  doc ("Rewrite terms like `let foo = bar in λx.baz` to `λx.let foo = bar in baz`, lifting lambda-bound variables"
    <> " above let-bound variables, recursively. This is helpful for targets such as Python.") $
  "term0" ~>
  "rewrite" <~ ("recurse" ~> "term" ~>
    "rewriteBinding" <~ ("b" ~> Core.bindingWithTerm (var "b") $ var "rewrite" @@ var "recurse" @@ Core.bindingTerm (var "b")) $
    "rewriteBindings" <~ ("bs" ~> Lists.map (var "rewriteBinding") (var "bs")) $
    "digForLambdas" <~ ("original" ~> "cons" ~> "term" ~> cases _Term (var"term")
      (Just $ var "recurse" @@ var "original") [
      _Term_annotated>>: "at" ~> var "digForLambdas"
        @@ var "original"
        @@ ("t" ~> Core.termAnnotated $ Core.annotatedTermWithBody (var "at") (var "cons" @@ var "t"))
        @@ (Core.annotatedTermBody $ var "at"),
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "recurse" @@ var "original") [
        _Function_lambda>>: "l" ~> Core.termFunction $ Core.functionLambda $ Core.lambdaWithBody (var "l") $
          var "digForLambdas"
            @@ (var "cons" @@ (Core.lambdaBody $ var "l"))
            @@ ("t" ~> var "cons" @@ var "t")
            @@ (Core.lambdaBody $ var "l")],
      _Term_let>>: "l" ~> var "digForLambdas"
        @@ var "original"
        @@ ("t" ~> var "cons" @@ (Core.termLet $ Core.let_ (var "rewriteBindings" @@ (Core.letBindings $ var "l")) (var "t")))
        @@ Core.letBody (var "l")]) $
    -- Note: we match *before* recursing for the sake of efficiency.
    cases _Term (var "term")
      (Just $ var "recurse" @@ var "term") [
      _Term_let>>: "l" ~> var "digForLambdas"
        @@ var "term"
        @@ ("t" ~> Core.termLet $ Core.let_ (var "rewriteBindings" @@ (Core.letBindings $ var "l")) (var "t"))
        @@ Core.letBody (var "l")]) $
  Rewriting.rewriteTerm @@ var "rewrite" @@ var "term0"

pruneLet :: TTermDefinition (Let -> Let)
pruneLet = define "pruneLet" $
  doc ("Given a let expression, remove any unused bindings. The resulting expression is still a let,"
    <> " even if has no remaining bindings") $
  "l" ~>
  "bindingMap" <~ Maps.fromList (Lists.map
    ("b" ~> pair (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")) $ Core.letBindings $ var "l") $
  "rootName" <~ Core.name (string "[[[root]]]") $
  "adj" <~ ("n" ~> Sets.intersection (Sets.fromList $ Maps.keys $ var "bindingMap")
      (Variables.freeVariablesInTerm @@ (Logic.ifElse (Equality.equal (var "n") (var "rootName"))
        (Core.letBody $ var "l")
        (Maybes.fromJust $ Maps.lookup (var "n") (var "bindingMap"))))) $
  "reachable" <~ Sorting.findReachableNodes @@ var "adj" @@ var "rootName" $
  "prunedBindings" <~ Lists.filter
    ("b" ~> Sets.member (Core.bindingName $ var "b") (var "reachable"))
    (Core.letBindings $ var "l") $
  Core.let_
    (var "prunedBindings")
    (Core.letBody $ var "l")

replaceTypedefs :: TTermDefinition (M.Map Name TypeScheme -> Type -> Type)
replaceTypedefs = define "replaceTypedefs" $
  doc "Replace all occurrences of simple typedefs (type aliases) with the aliased types, recursively" $
  "types" ~> "typ0" ~>
  "rewrite" <~ ("recurse" ~> "typ" ~>
    -- Note: dflt (recurse @@ typ) is NOT bound as a let here, because in strict languages (Java, Python)
    -- this would eagerly recurse into Record/Union/Wrap fields, causing infinite recursion on recursive types.
    -- Instead, we inline (recurse @@ typ) only where needed.
    cases _Type (var "typ")
      (Just $ var "recurse" @@ var "typ") [
--      _Type_forall>>: "ft" ~> ... -- TODO: shadowing via forall-bound variables
      _Type_annotated>>: "at" ~> Core.typeAnnotated $ Core.annotatedType
        (var "rewrite" @@ var "recurse" @@ (Core.annotatedTypeBody $ var "at"))
        (Core.annotatedTypeAnnotation $ var "at"),
      _Type_record>>: constant $ var "typ",
      _Type_union>>: constant $ var "typ",
      _Type_variable>>: "v" ~>
        "forMono" <~ ("t" ~> cases _Type (var "t")
          (Just $ var "rewrite" @@ var "recurse" @@ var "t") [
          _Type_record>>: constant $ var "typ",
          _Type_union>>: constant $ var "typ",
          _Type_wrap>>: constant $ var "typ"]) $
        "forTypeScheme" <~ ("ts" ~>
          "t" <~ Core.typeSchemeType (var "ts") $
          Logic.ifElse (Lists.null $ Core.typeSchemeVariables $ var "ts")
            (var "forMono" @@ var "t")
            (var "typ")) $ -- TODO: this may be too simple
        optCases (Maps.lookup (var "v") (var "types"))
        (var "typ")
        ("ts" ~> var "forTypeScheme" @@ var "ts"),
      _Type_wrap>>: constant $ var "typ"]) $
  Rewriting.rewriteType @@ var "rewrite" @@ var "typ0"

simplifyTerm :: TTermDefinition (Term -> Term)
simplifyTerm = define "simplifyTerm" $
  doc "Simplify terms by applying beta reduction where possible" $
  "term" ~>
  "simplify" <~ ("recurse" ~> "term" ~>
    "forRhs" <~ ("rhs" ~> "var" ~> "body" ~> cases _Term (Strip.deannotateTerm @@ var "rhs")
      (Just $ var "term") [
      _Term_variable>>: "v" ~>
        simplifyTerm @@ (Variables.substituteVariable @@ var "var" @@ var "v" @@ var "body")]) $
    "forLhs" <~ ("lhs" ~> "rhs" ~>
      "forFun" <~ ("fun" ~> cases _Function (var "fun")
        (Just $ var "term") [
        _Function_lambda>>: "l" ~>
          "var" <~ Core.lambdaParameter (var "l") $
          "body" <~ Core.lambdaBody (var "l") $
          Logic.ifElse (Sets.member (var "var") (Variables.freeVariablesInTerm @@ var "body"))
            (var "forRhs" @@ var "rhs" @@ var "var" @@ var "body")
            (simplifyTerm @@ var "body")]) $
      cases _Term (Strip.deannotateTerm @@ var "lhs")
        (Just $ var "term") [
        _Term_function>>: "fun" ~> var "forFun" @@ var "fun"]) $
    "forTerm" <~ ("stripped" ~> cases _Term (var "stripped")
      (Just $ var "term") [
      _Term_application>>: "app" ~>
        "lhs" <~ Core.applicationFunction (var "app") $
        "rhs" <~ Core.applicationArgument (var "app") $
        var "forLhs" @@ var "lhs" @@ var "rhs"]) $
    "stripped" <~ Strip.deannotateTerm @@ var "term" $
    var "recurse" @@ (var "forTerm" @@ var "stripped")) $
  Rewriting.rewriteTerm @@ var "simplify" @@ var "term"

termDependencyNames :: TTermDefinition (Bool -> Bool -> Bool -> Term -> S.Set Name)
termDependencyNames = define "termDependencyNames" $
  doc "Note: does not distinguish between bound and free variables; use freeVariablesInTerm for that" $
  "binds" ~> "withPrims" ~> "withNoms" ~> "term0" ~>
  "addNames" <~ ("names" ~> "term" ~>
    "nominal" <~ ("name" ~> Logic.ifElse (var "withNoms")
      (Sets.insert (var "name") (var "names"))
      (var "names")) $
    "prim" <~ ("name" ~> Logic.ifElse (var "withPrims")
      (Sets.insert (var "name") (var "names"))
      (var "names")) $
    "var" <~ ("name" ~> Logic.ifElse (var "binds")
      (Sets.insert (var "name") (var "names"))
      (var "names")) $
    cases _Term (var "term")
      (Just $ var "names") [
      _Term_function>>: "f" ~> cases _Function (var "f")
        (Just $ var "names") [
        _Function_primitive>>: "name" ~> var "prim" @@ var "name",
        _Function_elimination>>: "e" ~> cases _Elimination (var "e")
          Nothing [
          _Elimination_record>>: "proj" ~> var "nominal" @@ (Core.projectionTypeName $ var "proj"),
          _Elimination_union>>: "caseStmt" ~> var "nominal" @@ (Core.caseStatementTypeName $ var "caseStmt"),
          _Elimination_wrap>>: "name" ~> var "nominal" @@ var "name"]],
      _Term_record>>: "record" ~> var "nominal" @@ (Core.recordTypeName $ var "record"),
      _Term_union>>: "injection" ~> var "nominal" @@ (Core.injectionTypeName $ var "injection"),
      _Term_variable>>: "name" ~> var "var" @@ var "name",
      _Term_wrap>>: "wrappedTerm" ~> var "nominal" @@ (Core.wrappedTermTypeName $ var "wrappedTerm")]) $
  Rewriting.foldOverTerm @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty @@ var "term0"

toShortNames :: TTermDefinition ([Name] -> M.Map Name Name)
toShortNames = define "toShortNames" $
  doc "Generate short names from a list of fully qualified names" $
  "original" ~>
  "addName" <~ ("acc" ~> "name" ~>
    "local" <~ Names.localNameOf @@ var "name" $
    "group" <~ Maybes.fromMaybe Sets.empty (Maps.lookup (var "local") (var "acc")) $
    Maps.insert (var "local") (Sets.insert (var "name") (var "group")) (var "acc")) $
  "groupNamesByLocal" <~ ("names" ~> Lists.foldl (var "addName") Maps.empty (var "names")) $
  "groups" <~ var "groupNamesByLocal" @@ var "original" $
  "renameGroup" <~ ("localNames" ~>
    "local" <~ Pairs.first (var "localNames") $
    "names" <~ Pairs.second (var "localNames") $
    "rangeFrom" <~ ("start" ~> Lists.cons (var "start") (var "rangeFrom" @@ (Math.add (var "start") (int32 1)))) $
    "rename" <~ ("name" ~> "i" ~> pair (var "name") $ Core.name $
      Logic.ifElse (Equality.gt (var "i") (int32 1))
        (Strings.cat2 (var "local") (Literals.showInt32 $ var "i"))
        (var "local")) $
    Lists.zipWith (var "rename") (Sets.toList $ var "names") (var "rangeFrom" @@ int32 1)) $
  Maps.fromList $ Lists.concat $ Lists.map (var "renameGroup") $ Maps.toList $ var "groups"

topologicalSortBindingMap :: TTermDefinition (M.Map Name Term -> [[(Name, Term)]])
topologicalSortBindingMap = define "topologicalSortBindingMap" $
  doc "Topological sort of connected components, in terms of dependencies between variable/term binding pairs" $
  "bindingMap" ~>
  "bindings" <~ Maps.toList (var "bindingMap") $
  "keys" <~ Sets.fromList (Lists.map (unaryFunction Pairs.first) (var "bindings")) $
  -- TODO: this function currently serves no purpose; it always yields false
  "hasTypeAnnotation" <~ ("term" ~>
    cases _Term (var "term")
      (Just false) [
      _Term_annotated>>: "at" ~> var "hasTypeAnnotation" @@ (Core.annotatedTermBody $ var "at")]) $
  "depsOf" <~ ("nameAndTerm" ~>
    "name" <~ Pairs.first (var "nameAndTerm") $
    "term" <~ Pairs.second (var "nameAndTerm") $
    pair (var "name") $ Logic.ifElse (var "hasTypeAnnotation" @@ var "term")
      (list ([] :: [TTerm Name]))
      (Sets.toList $ Sets.intersection (var "keys") $ Variables.freeVariablesInTerm @@ var "term")) $
  "toPair" <~ ("name" ~> pair (var "name") $ Maybes.fromMaybe
    (Core.termLiteral $ Core.literalString $ string "Impossible!")
    (Maps.lookup (var "name") (var "bindingMap"))) $
  Lists.map (unaryFunction $ Lists.map $ var "toPair") (Sorting.topologicalSortComponents @@ Lists.map (var "depsOf") (var "bindings"))

topologicalSortBindings :: TTermDefinition ([Binding] -> Either [[Name]] [Name])
topologicalSortBindings = define "topologicalSortBindings" $
  doc "Topological sort of elements based on their dependencies" $
  "els" ~>
  "adjlist" <~ ("e" ~> pair
    (Core.bindingName $ var "e")
    (Sets.toList $ termDependencyNames @@ false @@ true @@ true @@ (Core.bindingTerm $ var "e"))) $
  Sorting.topologicalSort @@ Lists.map (var "adjlist") (var "els")

typeDependencyNames :: TTermDefinition (Bool -> Type -> S.Set Name)
typeDependencyNames = define "typeDependencyNames" $
  "withSchema" ~> "typ" ~> Logic.ifElse (var "withSchema")
    (Sets.union
      (Variables.freeVariablesInType @@ var "typ")
      (typeNamesInType @@ var "typ"))
    (Variables.freeVariablesInType @@ var "typ")

typeNamesInType :: TTermDefinition (Type -> S.Set Name)
typeNamesInType = define "typeNamesInType" $
  "typ0" ~>
  "addNames" <~ ("names" ~> "typ" ~> var "names") $
  Rewriting.foldOverType @@ Coders.traversalOrderPre @@ var "addNames" @@ Sets.empty @@ var "typ0"

elementsWithDependencies :: TTermDefinition (Context -> Graph -> [Binding] -> Either (InContext Error) [Binding])
elementsWithDependencies = define "elementsWithDependencies" $
  doc "Get elements with their dependencies" $
  "cx" ~> "graph" ~> "original" ~>
  "depNames" <~ ("el" ~> Sets.toList (termDependencyNames @@ true @@ false @@ false @@ (Core.bindingTerm (var "el")))) $
  "allDepNames" <~ Lists.nub (Lists.concat2
    (Lists.map (unaryFunction Core.bindingName) (var "original"))
    (Lists.concat (Lists.map (var "depNames") (var "original")))) $
  Eithers.mapList ("name" ~> Lexical.requireElement @@ var "cx" @@ var "graph" @@ var "name") (var "allDepNames")

topologicalSortTypeDefinitions :: TTermDefinition ([TypeDefinition] -> [[TypeDefinition]])
topologicalSortTypeDefinitions = define "topologicalSortTypeDefinitions" $
  doc "Topologically sort type definitions by dependencies" $
  "defs" ~>
  "toPair" <~ ("def" ~> pair
    (Module.typeDefinitionName (var "def"))
    (Sets.toList (typeDependencyNames @@ false @@ Module.typeDefinitionType (var "def")))) $
  "nameToDef" <~ Maps.fromList (Lists.map
    ("d" ~> pair (Module.typeDefinitionName (var "d")) (var "d"))
    (var "defs")) $
  "sorted" <~ Sorting.topologicalSortComponents @@ Lists.map (var "toPair") (var "defs") $
  Lists.map ("names" ~> Maybes.cat (Lists.map ("n" ~> Maps.lookup (var "n") (var "nameToDef")) (var "names"))) (
    var "sorted")
