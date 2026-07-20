{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Pg.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  defaultTinkerpopAnnotations, examplePgSchema, expString,
  lazyGraphToElements, pgElementToJson, pgElementsToJson,
  propertyGraphElements, typeApplicationTermToPropertyGraph)
import qualified Hydra.Dsl.Lib.Strings                as Strings
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Annotations                     as Annotations
import qualified Hydra.Overlay.Haskell.Bootstrap                       as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Overlay.Haskell.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Paths                      as Paths
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Errors                      as Error
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core                       as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Literals               as Literals
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Math                   as Math
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Packaging                     as Packaging
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Terms                      as MetaTerms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Variants                   as Variants
import qualified Hydra.Overlay.Haskell.Dsl.Prims                           as Prims
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Tabular                         as Tabular
import qualified Hydra.Overlay.Haskell.Dsl.Terms                           as Terms
import qualified Hydra.Overlay.Haskell.Dsl.Tests                           as Tests
import qualified Hydra.Overlay.Haskell.Dsl.Types                           as Types
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Print.Paths as PrintPaths
import qualified Hydra.Sources.Kernel.Terms.Print.Core      as PrintCore
import qualified Hydra.Sources.Kernel.Terms.Print.Graph     as PrintGraph
import qualified Hydra.Sources.Kernel.Terms.Print.Variants  as PrintVariants
import qualified Hydra.Sources.Kernel.Terms.Print.Typing    as PrintTyping
import qualified Hydra.Sources.Kernel.Terms.Sorting        as Sorting
import qualified Hydra.Sources.Kernel.Terms.Substitution   as Substitution
import qualified Hydra.Sources.Kernel.Terms.Templates      as Templates
import qualified Hydra.Sources.Kernel.Terms.Unification    as Unification
import qualified Hydra.Sources.Kernel.Types.All            as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                                  as I
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S
import qualified Data.Maybe                                as Y

-- Additional imports
import qualified Hydra.Pg.Model as PG
import qualified Hydra.Pg.Mapping as PGM
import qualified Hydra.Json.Model as JM
import qualified Hydra.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Sources.Pg.Coder as PgCoder
import qualified Hydra.Sources.Json.Model as JsonModel


define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

ns :: ModuleName
ns = ModuleName "hydra.pg.utils"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> ([ExtractCore.ns, PgCoder.ns, PrintCore.ns] L.++ (PgModel.ns:PgMapping.ns:JsonModel.ns:KernelTypes.kernelTypesModuleNames)),
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Utility functions for property graph operations")}
  where
    definitions = [
      toDefinition defaultTinkerpopAnnotations,
      toDefinition examplePgSchema,
      toDefinition expString,
      toDefinition lazyGraphToElements,
      toDefinition pgElementToJson,
      toDefinition pgElementsToJson,
      toDefinition (propertyGraphElements :: TypedTermDefinition (PG.Graph String -> [PG.Element String])),
      toDefinition typeApplicationTermToPropertyGraph]

-- | Default Tinkerpop annotation schema
defaultTinkerpopAnnotations :: TypedTermDefinition PGM.AnnotationSchema
defaultTinkerpopAnnotations = define "defaultTinkerpopAnnotations" $
  doc "Default Tinkerpop annotation schema" $
  record PGM._AnnotationSchema [
    PGM._AnnotationSchema_vertexLabel>>: string "vertexLabel",
    PGM._AnnotationSchema_edgeLabel>>: string "edgeLabel",
    PGM._AnnotationSchema_vertexId>>: string "vertexId",
    PGM._AnnotationSchema_edgeId>>: string "edgeId",
    PGM._AnnotationSchema_propertyKey>>: string "key",
    PGM._AnnotationSchema_propertyValue>>: string "value",
    PGM._AnnotationSchema_outVertex>>: string "outVertex",
    PGM._AnnotationSchema_outVertexLabel>>: string "outVertexLabel",
    PGM._AnnotationSchema_inVertex>>: string "inVertex",
    PGM._AnnotationSchema_inVertexLabel>>: string "inVertexLabel",
    PGM._AnnotationSchema_outEdge>>: string "outEdge",
    PGM._AnnotationSchema_outEdgeLabel>>: string "outEdgeLabel",
    PGM._AnnotationSchema_inEdge>>: string "inEdge",
    PGM._AnnotationSchema_inEdgeLabel>>: string "inEdgeLabel",
    PGM._AnnotationSchema_ignore>>: string "ignore"]

-- | Example property graph schema with string values
examplePgSchema :: TypedTermDefinition (PGM.Schema Graph () String Error)
examplePgSchema = define "examplePgSchema" $
  doc "Example property graph schema with string values" $
  record PGM._Schema [
    PGM._Schema_vertexIdTypes>>: Coders.coder (constant $ right unit) (constant $ right MetaTypes.unit),
    PGM._Schema_vertexIds>>: Coders.coder ("t" ~> expString @@ var "t") ("s" ~> right (Core.termLiteral $ Core.literalString $ var "s")),
    PGM._Schema_edgeIdTypes>>: Coders.coder (constant $ right unit) (constant $ right MetaTypes.unit),
    PGM._Schema_edgeIds>>: Coders.coder ("t" ~> expString @@ var "t") ("s" ~> right (Core.termLiteral $ Core.literalString $ var "s")),
    PGM._Schema_propertyTypes>>: Coders.coder (constant $ right unit) (constant $ right MetaTypes.unit),
    PGM._Schema_propertyValues>>: Coders.coder ("t" ~> expString @@ var "t") ("s" ~> right (Core.termLiteral $ Core.literalString $ var "s")),
    PGM._Schema_annotations>>: defaultTinkerpopAnnotations,
    PGM._Schema_defaultVertexId>>: string "defaultVertexId",
    PGM._Schema_defaultEdgeId>>: string "defaultEdgeId"]

-- | Extract a string from a term using the empty graph
expString :: TypedTermDefinition (Term -> Either Error String)
expString = define "expString" $
  doc "Extract a string from a term using the empty graph" $
  "term" ~>
    ExtractCore.string @@ Graph.emptyGraph @@ var "term"

-- | Get all elements from a lazy graph
lazyGraphToElements :: TypedTermDefinition (PG.LazyGraph v -> [PG.Element v])
lazyGraphToElements = define "lazyGraphToElements" $
  doc "Get all elements from a lazy graph" $
  "lg" ~>
    Lists.concat2
      (Lists.map ("x" ~> inject PG._Element PG._Element_vertex (var "x")) (project PG._LazyGraph PG._LazyGraph_vertices @@ var "lg"))
      (Lists.map ("x" ~> inject PG._Element PG._Element_edge (var "x")) (project PG._LazyGraph PG._LazyGraph_edges @@ var "lg"))

-- | Convert a property graph element to JSON
pgElementToJson :: TypedTermDefinition (PGM.Schema Graph t v e -> PG.Element v -> Either Error JM.Value)
pgElementToJson = define "pgElementToJson" $
  doc "Convert a property graph element to JSON" $
  "schema" ~> "el" ~>
    match PG._Element Nothing [
      PG._Element_vertex>>: "vertex" ~>
        Eithers.bind (Coders.coderDecode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ (project PG._Vertex PG._Vertex_id @@ var "vertex"))
          ("term" ~> lets [
            "labelJson">: Json.valueString (unwrap PG._VertexLabel @@ (project PG._Vertex PG._Vertex_label @@ var "vertex"))] $
            Eithers.map
              ("propsJson" ~>
                Json.valueObject (Optionals.cat $ list [
                  just (pair (string "label") (var "labelJson")),
                  just (pair (string "id") (Json.valueString $ PrintCore.term @@ var "term")),
                  var "propsJson"]))
              (propsToJson @@ var "schema" @@ (project PG._Vertex PG._Vertex_properties @@ var "vertex"))),
      PG._Element_edge>>: "edge" ~>
        Eithers.bind (Coders.coderDecode (project PGM._Schema PGM._Schema_edgeIds @@ var "schema") @@ (project PG._Edge PG._Edge_id @@ var "edge"))
          ("term" ~> Eithers.bind (Coders.coderDecode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ (project PG._Edge PG._Edge_out @@ var "edge"))
            ("termOut" ~> Eithers.bind (Coders.coderDecode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ (project PG._Edge PG._Edge_in @@ var "edge"))
              ("termIn" ~> lets [
                "labelJson">: Json.valueString (unwrap PG._EdgeLabel @@ (project PG._Edge PG._Edge_label @@ var "edge"))] $
                Eithers.map
                  ("propsJson" ~>
                    Json.valueObject (Optionals.cat $ list [
                      just (pair (string "label") (var "labelJson")),
                      just (pair (string "id") (Json.valueString $ PrintCore.term @@ var "term")),
                      just (pair (string "out") (Json.valueString $ PrintCore.term @@ var "termOut")),
                      just (pair (string "in") (Json.valueString $ PrintCore.term @@ var "termIn")),
                      var "propsJson"]))
                  (propsToJson @@ var "schema" @@ (project PG._Edge PG._Edge_properties @@ var "edge")))))]
    @@ var "el"

-- | Convert a list of property graph elements to JSON
pgElementsToJson :: TypedTermDefinition (PGM.Schema Graph t v e -> [PG.Element v] -> Either Error JM.Value)
pgElementsToJson = define "pgElementsToJson" $
  doc "Convert a list of property graph elements to JSON" $
  "schema" ~> "els" ~>
    Eithers.map ("els'" ~> Json.valueArray (var "els'")) (Eithers.mapList ("el" ~> pgElementToJson @@ var "schema" @@ var "el") (var "els"))

-- | Get all elements from a property graph
-- `Ord v` + `forall` because the graph's vertex/edge maps are keyed by the polymorphic vertex type
-- `v`, and the generated `Hydra.Dsl.Lib.Maps` exposes the primitive's `Ord` key constraint (the old
-- hand-written `Meta.Lib.Maps` did not). This also forces a placeholder concrete type at registration
-- in `definitions`; `v` is phantom/erased so the choice is arbitrary. See #467.
propertyGraphElements :: forall v. Ord v => TypedTermDefinition (PG.Graph v -> [PG.Element v])
propertyGraphElements = define "propertyGraphElements" $
  doc "Get all elements from a property graph" $
  "g" ~>
    Lists.concat2
      (Lists.map ("x" ~> inject PG._Element PG._Element_vertex (var "x")) (Maps.elems ((project PG._Graph PG._Graph_vertices @@ var "g") :: TypedTerm (M.Map v (PG.Vertex v)))))
      (Lists.map ("x" ~> inject PG._Element PG._Element_edge (var "x")) (Maps.elems ((project PG._Graph PG._Graph_edges @@ var "g") :: TypedTerm (M.Map v (PG.Edge v)))))

-- Internal helper (not exported as a binding)
propsToJson :: forall t v e. TypedTerm (PGM.Schema Graph t v e -> M.Map PG.PropertyKey v -> Either Error (Y.Maybe (String, JM.Value)))
propsToJson = "schema" ~> "pairs" ~>
  Logic.ifElse (Maps.null $ (var "pairs" :: TypedTerm (M.Map PG.PropertyKey v)))
    (right nothing)
    (Eithers.map
      ("p" ~> just (pair (string "properties") (Json.valueObject $ var "p")))
      (Eithers.mapList
        ("pair" ~> lets [
          "key">: Pairs.first $ var "pair",
          "v">: Pairs.second $ var "pair"] $
          Eithers.bind (Coders.coderDecode (project PGM._Schema PGM._Schema_propertyValues @@ var "schema") @@ var "v")
            ("term" ~> right (pair (unwrap PG._PropertyKey @@ var "key") (Json.valueString $ PrintCore.term @@ var "term"))))
        (Maps.toList $ (var "pairs" :: TypedTerm (M.Map PG.PropertyKey v)))))

-- | Convert a type-annotated term to property graph elements
typeApplicationTermToPropertyGraph :: TypedTermDefinition (PGM.Schema Graph t v e -> Type -> t -> t -> InferenceContext -> Graph
  -> Either Error (Term -> Either Error [PG.Element v]))
typeApplicationTermToPropertyGraph = define "typeApplicationTermToPropertyGraph" $
  doc "Convert a type-annotated term to property graph elements" $
  "schema" ~> "typ" ~> "vidType" ~> "eidType" ~> "cx" ~> "g" ~>
    Eithers.bind (PgCoder.elementCoder @@ nothing @@ var "schema" @@ var "typ" @@ var "vidType" @@ var "eidType" @@ var "cx" @@ var "g")
      ("adapter" ~> right
        ("term" ~>
          Eithers.map
            ("tree" ~> lets [
              "flattenTree">: "t" ~>
                Lists.cons
                  (project PG._ElementTree PG._ElementTree_self @@ var "t")
                  (Lists.concat (Lists.map (var "flattenTree") (project PG._ElementTree PG._ElementTree_dependencies @@ var "t")))]
              $ var "flattenTree" @@ var "tree")
            (Coders.coderEncode (Coders.adapterCoder $ var "adapter") @@ var "term")))
