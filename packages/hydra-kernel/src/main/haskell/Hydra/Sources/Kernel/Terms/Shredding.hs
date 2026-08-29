{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Terms.Shredding where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import Hydra.Overlay.Haskell.Libraries
import qualified Hydra.Dsl.Paths        as Paths
import qualified Hydra.Overlay.Haskell.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core         as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Lib.Chars    as Chars
import qualified Hydra.Dsl.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Logic    as Logic
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Math     as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Lib.Sets     as Sets
import qualified Hydra.Dsl.Lib.Strings  as Strings
import qualified Hydra.Overlay.Haskell.Dsl.Literals          as Literals
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base         as MetaBase
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms        as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types        as MetaTypes
import qualified Hydra.Dsl.Packaging       as Packaging
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Prims             as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular           as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing      as Testing
import qualified Hydra.Overlay.Haskell.Dsl.Terms             as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Dsl.Errors as Error


ns :: ModuleName
ns = ModuleName "hydra.shredding"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([Rewriting.ns] L.++ kernelTypesModuleNames),
            moduleMetadata = Bootstrap.descriptionMetadata (Just
              ("Shredding: the total, injective decomposition of a typed graph's bindings into flat,"
               <> " path-addressed links (edges, properties, attributes) — the link view of a graph."))}
  where
   definitions = [
     toDefinition shredGraph,
     toDefinition shredSchema,
     toDefinition shredTerm,
     toDefinition shredTermLinks,
     toDefinition shredType,
     toDefinition shredTypeLinks,
     toDefinition termAttributes,
     toDefinition typeAttributes]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

shredGraph :: TypedTermDefinition (Graph -> Prelude.Either Error SubtermGraph)
shredGraph = define "shredGraph" $
  doc ("The link view of a typed graph: one node per bound term, each with the edge/property/attribute"
    <> " links found by a complete traversal of its term. Input must be typed (binding type schemes"
    <> " present); an untyped binding is a precondition failure, not a degraded mode.") $
  "graph" ~>
  "boundTerms" <~ Graph.graphBoundTerms (var "graph") $
  "boundTypes" <~ Graph.graphBoundTypes (var "graph") $
  Eithers.map ("nodes" ~> Paths.subtermGraph (var "nodes")) (Eithers.mapList
    ("nt" ~>
      "name" <~ Pairs.first (var "nt") $
      "term" <~ Pairs.second (var "nt") $
      Optionals.match (Maps.lookup (var "name" :: TypedTerm Name) (var "boundTypes"))
        (left (Error.errorOther $ Error.otherError $ Strings.concat2 (string "shredGraph: untyped binding: ") (Core.unName $ var "name")))
        ("ts" ~> shredTerm @@ var "graph" @@ var "name" @@ var "ts" @@ var "term"))
    (Maps.toList (var "boundTerms" :: TypedTerm (M.Map Name Term))))

shredSchema :: TypedTermDefinition (M.Map Name Type -> Prelude.Either Error SubtypeGraph)
shredSchema = define "shredSchema" $
  doc ("The link view of a schema: one node per named type. A Type.variable referencing a schema-bound"
    <> " name is an edge; a forall-bound variable in scope is a property; leaf types are properties.") $
  "schema" ~>
  -- The set of schema-bound names, as a map, for O(log n) edge-classification membership tests.
  "schemaNames" <~ (Maps.fromList (Lists.map
    ("nt" ~> pair (Pairs.first $ var "nt") (Core.typeScheme (list ([] :: [TypedTerm Name])) (Pairs.second $ var "nt") Maps.empty))
    (Maps.toList (var "schema" :: TypedTerm (M.Map Name Type)))) :: TypedTerm (M.Map Name TypeScheme)) $
  Eithers.map ("nodes" ~> Paths.subtypeGraph (var "nodes")) (Eithers.mapList
    ("nt" ~> shredType @@ var "schemaNames" @@ (Pairs.first $ var "nt") @@ (Pairs.second $ var "nt"))
    (Maps.toList (var "schema" :: TypedTerm (M.Map Name Type))))

shredTerm :: TypedTermDefinition (Graph -> Name -> TypeScheme -> Term -> Prelude.Either Error SubtermNode)
shredTerm = define "shredTerm" $
  doc "Shred one binding (name, type scheme, term) of a graph into a subterm node" $
  "graph" ~> "name" ~> "ts" ~> "term" ~>
  Eithers.map
    ("links" ~> Paths.subtermNode (var "name") (var "ts") (var "links"))
    (shredTermLinks @@ var "graph" @@ (Sets.empty :: TypedTerm (S.Set Name)) @@ (Paths.subtermPath $ list ([] :: [TypedTerm SubtermStep])) @@ var "term")

shredTermLinks :: TypedTermDefinition (Graph -> S.Set Name -> SubtermPath -> Term -> Prelude.Either Error [SubtermLink])
shredTermLinks = define "shredTermLinks" $
  doc ("Compute the outgoing links of a term at the given path, given the local scope of names bound"
    <> " by lambda/let steps already taken. Leaf terms yield a property or edge; a variable is classified"
    <> " as an edge (bound in the graph), a property (locally bound or a primitive), or a failure (free).") $
  "graph" ~> "scope" ~> "path" ~> "term" ~>
  "steps" <~ unwrap _SubtermPath @@ var "path" $
  -- Attributes contributed by this constructor
  "attrs" <~ Lists.map (reify Paths.subtermLinkAttribute) (termAttributes @@ var "path" @@ var "term") $
  -- The scope extended by descending into this term's children (lambda parameter / let binding names)
  "childScope" <~ (match _Term (var "term") (Just $ var "scope") [
    _Term_lambda>>: "l" ~> Sets.insert (Core.lambdaParameter $ var "l") (var "scope"),
    _Term_let>>: "lt" ~> Lists.foldl
      ("acc" ~> "b" ~> Sets.insert (Core.bindingName $ var "b") (var "acc"))
      (var "scope")
      (Core.letBindings $ var "lt")]) $
  -- This term's own leaf link, if it is a leaf (Either Error [SubtermLink])
  "leafLinks" <~ match _Term (var "term") (Just $ right (list ([] :: [TypedTerm SubtermLink]))) [
    _Term_literal>>: constant $ right (list [Paths.subtermLinkProperty $ Paths.subtermProperty (var "path") (var "term")]),
    _Term_project>>: constant $ right (list [Paths.subtermLinkProperty $ Paths.subtermProperty (var "path") (var "term")]),
    _Term_unit>>: constant $ right (list [Paths.subtermLinkProperty $ Paths.subtermProperty (var "path") (var "term")]),
    _Term_unwrap>>: constant $ right (list [Paths.subtermLinkProperty $ Paths.subtermProperty (var "path") (var "term")]),
    _Term_variable>>: "name" ~>
      Logic.ifElse (Sets.member (var "name" :: TypedTerm Name) (var "scope"))
        -- locally bound (lambda / inner let) -> property
        (right (list [Paths.subtermLinkProperty $ Paths.subtermProperty (var "path") (var "term")]))
        (Logic.ifElse (Maps.member (var "name" :: TypedTerm Name) (Graph.graphBoundTerms $ var "graph"))
          -- bound in the graph -> edge
          (right (list [Paths.subtermLinkEdge $ Paths.subtermEdge (var "path") (var "name")]))
          (Logic.ifElse (Maps.member (var "name" :: TypedTerm Name) (Graph.graphPrimitives $ var "graph"))
            -- a primitive -> property
            (right (list [Paths.subtermLinkProperty $ Paths.subtermProperty (var "path") (var "term")]))
            -- otherwise free -> precondition failure
            (left (Error.errorOther $ Error.otherError $ Strings.concat2 (string "free variable in shredded term: ") (Core.unName $ var "name")))))] $
  -- Recurse into the children, sequencing errors
  Eithers.bind (Eithers.mapList
    ("st" ~>
      "step" <~ Pairs.first (var "st") $
      "child" <~ Pairs.second (var "st") $
      "childPath" <~ Paths.subtermPath (Lists.concat2 (var "steps") (list [var "step"])) $
      shredTermLinks @@ var "graph" @@ var "childScope" @@ var "childPath" @@ var "child")
    (Rewriting.subtermsWithSteps @@ var "term"))
    ("childLinkLists" ~>
      Eithers.bind (var "leafLinks")
        ("leaf" ~> right (Lists.concat2 (var "attrs") (Lists.concat2 (var "leaf") (Lists.concat (var "childLinkLists"))))))

shredType :: TypedTermDefinition (M.Map Name TypeScheme -> Name -> Type -> Prelude.Either Error SubtypeNode)
shredType = define "shredType" $
  doc "Shred one named type of a schema into a subtype node" $
  "schema" ~> "name" ~> "typ" ~>
  Eithers.map
    ("links" ~> Paths.subtypeNode (var "name") (var "links"))
    (shredTypeLinks @@ var "schema" @@ (Sets.empty :: TypedTerm (S.Set Name)) @@ (Paths.subtypePath $ list ([] :: [TypedTerm SubtypeStep])) @@ var "typ")

shredTypeLinks :: TypedTermDefinition (M.Map Name TypeScheme -> S.Set Name -> SubtypePath -> Type -> Prelude.Either Error [SubtypeLink])
shredTypeLinks = define "shredTypeLinks" $
  doc ("Compute the outgoing links of a type at the given path, given the scope of forall-bound"
    <> " variables. A leaf type yields a property; a variable is an edge (a named type in the schema),"
    <> " a property (forall-bound in scope), or a failure (free).") $
  "schema" ~> "scope" ~> "path" ~> "typ" ~>
  "steps" <~ unwrap _SubtypePath @@ var "path" $
  "attrs" <~ Lists.map (reify Paths.subtypeLinkAttribute) (typeAttributes @@ var "path" @@ var "typ") $
  "childScope" <~ (match _Type (var "typ") (Just $ var "scope") [
    _Type_forall>>: "ft" ~> Sets.insert (Core.forallTypeParameter $ var "ft") (var "scope")]) $
  "leafLinks" <~ match _Type (var "typ") (Just $ right (list ([] :: [TypedTerm SubtypeLink]))) [
    _Type_literal>>: constant $ right (list [Paths.subtypeLinkProperty $ Paths.subtypeProperty (var "path") (var "typ")]),
    _Type_unit>>: constant $ right (list [Paths.subtypeLinkProperty $ Paths.subtypeProperty (var "path") (var "typ")]),
    _Type_void>>: constant $ right (list [Paths.subtypeLinkProperty $ Paths.subtypeProperty (var "path") (var "typ")]),
    _Type_variable>>: "name" ~>
      Logic.ifElse (Sets.member (var "name" :: TypedTerm Name) (var "scope"))
        (right (list [Paths.subtypeLinkProperty $ Paths.subtypeProperty (var "path") (var "typ")]))
        (Logic.ifElse (Maps.member (var "name" :: TypedTerm Name) (var "schema"))
          (right (list [Paths.subtypeLinkEdge $ Paths.subtypeEdge (var "path") (var "name")]))
          (left (Error.errorOther $ Error.otherError $ Strings.concat2 (string "free type variable in shredded type: ") (Core.unName $ var "name"))))] $
  Eithers.bind (Eithers.mapList
    ("st" ~>
      "step" <~ Pairs.first (var "st") $
      "child" <~ Pairs.second (var "st") $
      "childPath" <~ Paths.subtypePath (Lists.concat2 (var "steps") (list [var "step"])) $
      shredTypeLinks @@ var "schema" @@ var "childScope" @@ var "childPath" @@ var "child")
    (Rewriting.subtypesWithSteps @@ var "typ"))
    ("childLinkLists" ~>
      Eithers.bind (var "leafLinks")
        ("leaf" ~> right (Lists.concat2 (var "attrs") (Lists.concat2 (var "leaf") (Lists.concat (var "childLinkLists"))))))

termAttributes :: TypedTermDefinition (SubtermPath -> Term -> [SubtermAttribute])
termAttributes = define "termAttributes" $
  doc "The attribute links contributed by a term constructor at the given path" $
  "path" ~> "term" ~>
  "attr" <~ ("a" ~> Paths.subtermAttribute (var "path") (var "a")) $
  "one" <~ ("a" ~> list [var "attr" @@ var "a"]) $
  match _Term (var "term")
    (Just $ list ([] :: [TypedTerm SubtermAttribute])) [
    _Term_cases>>: "cs" ~> var "one" @@ (Paths.termAttributeCasesTypeName $ Core.caseStatementTypeName $ var "cs"),
    _Term_inject>>: "inj" ~> var "one" @@ (Paths.termAttributeInjectTypeName $ Core.injectionTypeName $ var "inj"),
    _Term_lambda>>: "l" ~> Lists.concat2
      (list [var "attr" @@ (Paths.termAttributeLambdaParameter $ Core.lambdaParameter $ var "l")])
      (Optionals.match (Core.lambdaDomain $ var "l")
        (list ([] :: [TypedTerm SubtermAttribute]))
        ("d" ~> list [var "attr" @@ (Paths.termAttributeLambdaDomainGiven $ var "d")])),
    _Term_project>>: "p" ~> list [
      var "attr" @@ (Paths.termAttributeProjectTypeName $ Core.projectionTypeName $ var "p"),
      var "attr" @@ (Paths.termAttributeProjectFieldName $ Core.projectionFieldName $ var "p")],
    _Term_record>>: "r" ~> var "one" @@ (Paths.termAttributeRecordTypeName $ Core.recordTypeName $ var "r"),
    _Term_typeApplication>>: "ta" ~> var "one" @@ (Paths.termAttributeTypeApplicationType $ Core.typeApplicationTermType $ var "ta"),
    _Term_typeLambda>>: "tl" ~> var "one" @@ (Paths.termAttributeTypeLambdaParameter $ Core.typeLambdaParameter $ var "tl"),
    _Term_wrap>>: "w" ~> var "one" @@ (Paths.termAttributeWrapTypeName $ Core.wrappedTermTypeName $ var "w")]

typeAttributes :: TypedTermDefinition (SubtypePath -> Type -> [SubtypeAttribute])
typeAttributes = define "typeAttributes" $
  doc "The attribute links contributed by a type constructor at the given path" $
  "path" ~> "typ" ~>
  "attr" <~ ("a" ~> Paths.subtypeAttribute (var "path") (var "a")) $
  match _Type (var "typ")
    (Just $ list ([] :: [TypedTerm SubtypeAttribute])) [
    _Type_annotated>>: "at" ~> list [var "attr" @@ (Paths.typeAttributeAnnotatedAnnotation $ Core.annotatedTypeAnnotation $ var "at")],
    _Type_forall>>: "ft" ~> list [var "attr" @@ (Paths.typeAttributeForallParameter $ Core.forallTypeParameter $ var "ft")]]
