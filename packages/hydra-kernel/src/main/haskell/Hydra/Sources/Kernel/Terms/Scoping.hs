
module Hydra.Sources.Kernel.Terms.Scoping where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  extendGraphForLambda,
  extendGraphForLet,
  extendGraphForTypeLambda,
  extendGraphWithBindings,
  fTypeToTypeScheme,
  termSignatureToTypeScheme,
  typeSchemeToFType,
  typeSchemeToTermSignature)
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
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
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
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Sources.Kernel.Terms.Sorting as Sorting


ns :: Namespace
ns = Namespace "hydra.scoping"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInNamespace ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleDependencies = kernelTypesNamespaces,
            moduleDescription = Just ("Graph context extension and type scheme conversion")}
  where
   definitions = [
     toDefinition extendGraphForLambda,
     toDefinition extendGraphForLet,
     toDefinition extendGraphForTypeLambda,
     toDefinition extendGraphWithBindings,
     toDefinition fTypeToTypeScheme,
     toDefinition termSignatureToTypeScheme,
     toDefinition typeSchemeToFType,
     toDefinition typeSchemeToTermSignature]

fTypeToTypeScheme :: TTermDefinition (Type -> TypeScheme)
fTypeToTypeScheme = define "fTypeToTypeScheme" $
  doc "Convert a forall type to a type scheme" $
  "typ" ~>
  "stripAnnotations" <~ ("t" ~> cases _Type (var "t")
    (Just $ var "t") [
    _Type_annotated>>: "at" ~> var "stripAnnotations" @@ (Core.annotatedTypeBody $ var "at")]) $
  "gatherForall" <~ ("vars" ~> "typ" ~> cases _Type (var "stripAnnotations" @@ var "typ")
     (Just $ Core.typeScheme (Lists.reverse $ var "vars") (var "typ") Phantoms.nothing) [
     _Type_forall>>: "ft" ~> var "gatherForall" @@
       (Lists.cons (Core.forallTypeParameter $ var "ft") (var "vars")) @@
       (Core.forallTypeBody $ var "ft")]) $
  var "gatherForall" @@ list ([] :: [TTerm Name]) @@ var "typ"

typeSchemeToFType :: TTermDefinition (TypeScheme -> Type)
typeSchemeToFType = define "typeSchemeToFType" $
  doc "Convert a type scheme to a forall type" $
  "ts" ~>
  "vars" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeBody (var "ts") $
  Lists.foldl
    ("t" ~> "v" ~> Core.typeForall $ Core.forallType (var "v") (var "t"))
    (var "body")
    (Lists.reverse $ var "vars")

termSignatureToTypeScheme :: TTermDefinition (TermSignature -> TypeScheme)
termSignatureToTypeScheme = define "termSignatureToTypeScheme" $
  doc "Convert a TermSignature to a TypeScheme, erasing parameter names, descriptions, and laziness flags." $
  "sig" ~>
  "typeParams" <~ Typing.termSignatureTypeParameters (var "sig") $
  "params" <~ Typing.termSignatureParameters (var "sig") $
  "result" <~ Typing.termSignatureResult (var "sig") $
  "variables" <~ Lists.map ("tp" ~> Typing.typeParameterName (var "tp")) (var "typeParams") $
  -- Right-fold the parameter types into a chain of Function arrows, terminating in the result type.
  "body" <~ Lists.foldl
    ("acc" ~> "p" ~> Core.typeFunction $ Core.functionType (Typing.parameterType $ var "p") (var "acc"))
    (Typing.resultType $ var "result")
    (Lists.reverse $ var "params") $
  -- Build the optional constraints map. If no type parameter carries any constraints, emit nothing;
  -- otherwise build a map from each type parameter's name to its TypeVariableMetadata.
  "hasConstraints" <~ Lists.foldl
    ("acc" ~> "tp" ~> Logic.or (var "acc") (Logic.not $ Lists.null $ Typing.typeParameterConstraints $ var "tp"))
    false
    (var "typeParams") $
  "constraints" <~ Logic.ifElse (var "hasConstraints")
    (Phantoms.just $ Maps.fromList $ Lists.map
      ("tp" ~> pair (Typing.typeParameterName $ var "tp")
        (Core.typeVariableMetadata $ Sets.fromList $ Typing.typeParameterConstraints $ var "tp"))
      (var "typeParams"))
    Phantoms.nothing $
  Core.typeScheme (var "variables") (var "body") (var "constraints")

typeSchemeToTermSignature :: TTermDefinition (TypeScheme -> TermSignature)
typeSchemeToTermSignature = define "typeSchemeToTermSignature" $
  doc ("Convert a TypeScheme to a TermSignature. Type variables and class constraints are preserved exactly."
    <> " Value-parameter names are synthesized as arg0, arg1, .... Per-parameter descriptions are nothing"
    <> " and isLazy defaults to false.") $
  "ts" ~>
  "variables" <~ Core.typeSchemeVariables (var "ts") $
  "body" <~ Core.typeSchemeBody (var "ts") $
  "constraintsMap" <~ Maybes.fromMaybe Maps.empty (Core.typeSchemeConstraints $ var "ts") $
  -- Build TypeParameters, looking up each variable's class constraints in the constraints map.
  "typeParams" <~ Lists.map
    ("v" ~> Typing.typeParameter (var "v") $ optCases
      (Maps.lookup (var "v") (var "constraintsMap"))
      (list ([] :: [TTerm TypeClassConstraint]))
      ("tvm" ~> Sets.toList $ Core.typeVariableMetadataClasses $ var "tvm"))
    (var "variables") $
  -- Peel function arrows off the body, accumulating parameter types in reverse order.
  "peel" <~ ("acc" ~> "t" ~> cases _Type (var "t")
    (Just $ pair (Lists.reverse $ var "acc") (var "t")) [
    _Type_function>>: "ft" ~> var "peel" @@
      (Lists.cons (Core.functionTypeDomain $ var "ft") (var "acc")) @@
      (Core.functionTypeCodomain $ var "ft")]) $
  "peeled" <~ (var "peel" @@ list ([] :: [TTerm Type]) @@ var "body") $
  "paramTypes" <~ Pairs.first (var "peeled") $
  "resultType" <~ Pairs.second (var "peeled") $
  -- Build Parameters with synthetic names arg0, arg1, .... We iterate via
  -- (length-1)-indexed fold to assign each parameter an index, since there is
  -- no DSL `range` helper.
  "params" <~ Lists.reverse (Pairs.first $ Lists.foldl
    ("acc" ~> "ty" ~>
      "pairAcc" <~ Pairs.first (var "acc") $
      "i" <~ Pairs.second (var "acc") $
      pair
        (Lists.cons
          (Typing.parameter
            (Core.name $ Strings.cat (list [string "arg", Literals.showInt32 (var "i")]))
            Phantoms.nothing
            (var "ty")
            false)
          (var "pairAcc"))
        (Math.add (var "i") (int32 1)))
    (pair (list ([] :: [TTerm Parameter])) (int32 0))
    (var "paramTypes")) $
  "result" <~ Typing.result Phantoms.nothing (var "resultType") $
  Typing.termSignature (var "typeParams") (var "params") (var "result")

extendGraphForLambda :: TTermDefinition (Graph -> Lambda -> Graph)
extendGraphForLambda = define "extendGraphForLambda" $
  doc "Extend a graph by descending into a lambda body" $
  "g" ~> "lam" ~>
  "var" <~ Core.lambdaParameter (var "lam") $
  Graph.graph
    (Graph.graphBoundTerms $ var "g")
    (optCases (Core.lambdaDomain $ var "lam")
      (Graph.graphBoundTypes $ var "g")
      ("dom" ~> Maps.insert (var "var") (fTypeToTypeScheme @@ var "dom") $ Graph.graphBoundTypes $ var "g"))
    (Graph.graphClassConstraints $ var "g")
    (Sets.insert (var "var") $ Graph.graphLambdaVariables $ var "g")
    (Maps.delete (var "var") $ Graph.graphMetadata $ var "g")
    (Graph.graphPrimitives $ var "g")
    (Graph.graphSchemaTypes $ var "g")
    (Graph.graphTypeVariables $ var "g")

extendGraphForLet :: TTermDefinition ((Graph -> Binding -> Maybe Term) -> Graph -> Let -> Graph)
extendGraphForLet = define "extendGraphForLet" $
  doc "Extend a graph by descending into a let body" $
  "forBinding" ~> "g" ~> "letrec" ~>
  "bindings" <~ Core.letBindings (var "letrec") $
  -- Pre-extend graph with sibling bindings so forBinding can resolve them
  "g2" <~ (extendGraphWithBindings @@ var "bindings" @@ var "g") $
  Graph.graph
    -- Add all binding terms
    (Maps.union
      (Maps.fromList $ Lists.map ("b" ~> pair (Core.bindingName $ var "b") (Core.bindingTerm $ var "b")) (var "bindings"))
      (Graph.graphBoundTerms $ var "g"))
    -- Add typed binding type schemes; untyped bindings are not added, so outer types are shadowed by union precedence
    (Maps.union
      (Maps.fromList $ Maybes.cat $ Lists.map
        ("b" ~> Maybes.map ("ts" ~> pair (Core.bindingName $ var "b") (var "ts"))
          (Core.bindingTypeScheme $ var "b"))
        (var "bindings"))
      (Graph.graphBoundTypes $ var "g"))
    (Graph.graphClassConstraints $ var "g")
    -- Remove all binding names from lambda variables; they are shadowed
    (Lists.foldl ("s" ~> "b" ~> Sets.delete (Core.bindingName $ var "b") (var "s"))
      (Graph.graphLambdaVariables $ var "g")
      (var "bindings"))
    -- Update metadata per binding, accumulating a full graph so each binding sees earlier siblings' metadata
    (Graph.graphMetadata $ Lists.foldl
      ("gAcc" ~> "b" ~>
        "m" <~ (Graph.graphMetadata $ var "gAcc") $
        "newMeta" <~ (optCases (var "forBinding" @@ var "gAcc" @@ var "b")
          (Maps.delete (Core.bindingName $ var "b") (var "m"))
          ("t" ~> Maps.insert (Core.bindingName $ var "b") (var "t") (var "m"))) $
        Graph.graph
          (Graph.graphBoundTerms $ var "gAcc")
          (Graph.graphBoundTypes $ var "gAcc")
          (Graph.graphClassConstraints $ var "gAcc")
          (Graph.graphLambdaVariables $ var "gAcc")
          (var "newMeta")
          (Graph.graphPrimitives $ var "gAcc")
          (Graph.graphSchemaTypes $ var "gAcc")
          (Graph.graphTypeVariables $ var "gAcc"))
      (var "g2")
      (var "bindings"))
    (Graph.graphPrimitives $ var "g")
    (Graph.graphSchemaTypes $ var "g")
    (Graph.graphTypeVariables $ var "g")

extendGraphForTypeLambda :: TTermDefinition (Graph -> TypeLambda -> Graph)
extendGraphForTypeLambda = define "extendGraphForTypeLambda" $
  doc "Extend a graph by descending into a type lambda body" $
  "g" ~> "tlam" ~>
  "name" <~ Core.typeLambdaParameter (var "tlam") $
  Graph.graph
    (Graph.graphBoundTerms $ var "g")
    (Graph.graphBoundTypes $ var "g")
    (Graph.graphClassConstraints $ var "g")
    (Graph.graphLambdaVariables $ var "g")
    (Graph.graphMetadata $ var "g")
    (Graph.graphPrimitives $ var "g")
    (Graph.graphSchemaTypes $ var "g")
    (Sets.insert (var "name") $ Graph.graphTypeVariables $ var "g")

extendGraphWithBindings :: TTermDefinition ([Binding] -> Graph -> Graph)
extendGraphWithBindings = define "extendGraphWithBindings" $
  doc "Add bindings to an existing graph" $
  "bindings" ~> "g" ~>
  -- Merge new binding terms/types into existing graph
  "newTerms" <~ Maps.fromList (Lists.map ("b" ~>
    pair (Core.bindingName (var "b")) (Core.bindingTerm (var "b"))) (var "bindings")) $
  "newTypes" <~ Maps.fromList (Maybes.cat (Lists.map ("b" ~>
    Maybes.map ("ts" ~> pair (Core.bindingName (var "b")) (var "ts"))
      (Core.bindingTypeScheme (var "b"))) (var "bindings"))) $
  Graph.graph
    (Maps.union (var "newTerms") (Graph.graphBoundTerms (var "g")))
    (Maps.union (var "newTypes") (Graph.graphBoundTypes (var "g")))
    (Graph.graphClassConstraints (var "g"))
    (Graph.graphLambdaVariables (var "g"))
    (Graph.graphMetadata (var "g"))
    (Graph.graphPrimitives (var "g"))
    (Graph.graphSchemaTypes (var "g"))
    (Graph.graphTypeVariables (var "g"))
