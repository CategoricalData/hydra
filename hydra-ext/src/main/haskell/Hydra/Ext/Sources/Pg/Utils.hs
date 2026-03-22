module Hydra.Ext.Sources.Pg.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  defaultTinkerpopAnnotations, examplePgSchema, expString,
  lazyGraphToElements, pgElementToJson, pgElementsToJson,
  propertyGraphElements, typeApplicationTermToPropertyGraph)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Accessors                  as Accessors
import qualified Hydra.Dsl.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Coders                     as Coders
import qualified Hydra.Dsl.Util                    as Util
import qualified Hydra.Dsl.Meta.Context                    as Ctx
import qualified Hydra.Dsl.Error                      as Error
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Json.Model                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Typing                     as Typing
import qualified Hydra.Dsl.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Meta.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.All            as KernelTerms
import qualified Hydra.Sources.Kernel.Terms.Annotations    as Annotations
import qualified Hydra.Sources.Kernel.Terms.Arity          as Arity
import qualified Hydra.Sources.Kernel.Terms.Checking       as Checking
import qualified Hydra.Sources.Kernel.Terms.Constants      as Constants
import qualified Hydra.Sources.Kernel.Terms.Extract.Core   as ExtractCore
import qualified Hydra.Sources.Kernel.Terms.Extract.Util   as ExtractUtil
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting
import qualified Hydra.Sources.Kernel.Terms.Grammars       as Grammars
import qualified Hydra.Sources.Kernel.Terms.Inference      as Inference
import qualified Hydra.Sources.Kernel.Terms.Languages      as Languages
import qualified Hydra.Sources.Kernel.Terms.Lexical        as Lexical
import qualified Hydra.Sources.Kernel.Terms.Literals       as Literals
import qualified Hydra.Sources.Kernel.Terms.Names          as Names
import qualified Hydra.Sources.Kernel.Terms.Reduction      as Reduction
import qualified Hydra.Sources.Kernel.Terms.Reflect        as Reflect
import qualified Hydra.Sources.Kernel.Terms.Rewriting      as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas        as Schemas
import qualified Hydra.Sources.Kernel.Terms.Serialization  as Serialization
import qualified Hydra.Sources.Kernel.Terms.Show.Accessors as ShowAccessors
import qualified Hydra.Sources.Kernel.Terms.Show.Core      as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Graph     as ShowGraph
import qualified Hydra.Sources.Kernel.Terms.Show.Meta      as ShowMeta
import qualified Hydra.Sources.Kernel.Terms.Show.Typing    as ShowTyping
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
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Ext.Sources.Pg.Mapping as PgMapping
import qualified Hydra.Ext.Sources.Pg.Coder as PgCoder
import qualified Hydra.Sources.Json.Model as JsonModel


define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

ns :: Namespace
ns = Namespace "hydra.pg.utils"

module_ :: Module
module_ = Module ns elements
    [ExtractCore.ns, PgCoder.ns, Schemas.ns]
    (PgModel.ns:PgMapping.ns:JsonModel.ns:KernelTypes.kernelTypesNamespaces) $
    Just "Utility functions for property graph operations"
  where
    elements = [
      toBinding defaultTinkerpopAnnotations,
      toBinding examplePgSchema,
      toBinding expString,
      toBinding lazyGraphToElements,
      toBinding pgElementToJson,
      toBinding pgElementsToJson,
      toBinding propertyGraphElements,
      toBinding typeApplicationTermToPropertyGraph]

-- | Default Tinkerpop annotation schema
defaultTinkerpopAnnotations :: TBinding PGM.AnnotationSchema
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
examplePgSchema :: TBinding (PGM.Schema Graph () String)
examplePgSchema = define "examplePgSchema" $
  doc "Example property graph schema with string values" $
  record PGM._Schema [
    PGM._Schema_vertexIdTypes>>: Util.coder (constant $ constant $ right unit) (constant $ constant $ right MetaTypes.unit),
    PGM._Schema_vertexIds>>: Util.coder ("cx" ~> "t" ~> expString @@ var "cx" @@ var "t") ("_cx" ~> "s" ~> right (Core.termLiteral $ Core.literalString $ var "s")),
    PGM._Schema_edgeIdTypes>>: Util.coder (constant $ constant $ right unit) (constant $ constant $ right MetaTypes.unit),
    PGM._Schema_edgeIds>>: Util.coder ("cx" ~> "t" ~> expString @@ var "cx" @@ var "t") ("_cx" ~> "s" ~> right (Core.termLiteral $ Core.literalString $ var "s")),
    PGM._Schema_propertyTypes>>: Util.coder (constant $ constant $ right unit) (constant $ constant $ right MetaTypes.unit),
    PGM._Schema_propertyValues>>: Util.coder ("cx" ~> "t" ~> expString @@ var "cx" @@ var "t") ("_cx" ~> "s" ~> right (Core.termLiteral $ Core.literalString $ var "s")),
    PGM._Schema_annotations>>: defaultTinkerpopAnnotations,
    PGM._Schema_defaultVertexId>>: string "defaultVertexId",
    PGM._Schema_defaultEdgeId>>: string "defaultEdgeId"]

-- | Extract a string from a term using the empty graph
expString :: TBinding (Context -> Term -> Either (InContext Error) String)
expString = define "expString" $
  doc "Extract a string from a term using the empty graph" $
  "cx" ~> "term" ~>
    ExtractCore.string @@ var "cx" @@ Graph.emptyGraph @@ var "term"

-- | Get all elements from a property graph
propertyGraphElements :: TBinding (PG.Graph v -> [PG.Element v])
propertyGraphElements = define "propertyGraphElements" $
  doc "Get all elements from a property graph" $
  "g" ~>
    Lists.concat2
      (Lists.map ("x" ~> inject PG._Element PG._Element_vertex (var "x")) (Maps.elems $ project PG._Graph PG._Graph_vertices @@ var "g"))
      (Lists.map ("x" ~> inject PG._Element PG._Element_edge (var "x")) (Maps.elems $ project PG._Graph PG._Graph_edges @@ var "g"))

-- | Convert a type-annotated term to property graph elements
typeApplicationTermToPropertyGraph :: TBinding (PGM.Schema Graph t v -> Type -> t -> t -> Context -> Graph
  -> Either (InContext Error) (Term -> Context -> Either (InContext Error) [PG.Element v]))
typeApplicationTermToPropertyGraph = define "typeApplicationTermToPropertyGraph" $
  doc "Convert a type-annotated term to property graph elements" $
  "schema" ~> "typ" ~> "vidType" ~> "eidType" ~> "cx" ~> "g" ~>
    Eithers.bind (PgCoder.elementCoder @@ nothing @@ var "schema" @@ var "typ" @@ var "vidType" @@ var "eidType" @@ var "cx" @@ var "g")
      ("adapter" ~> right
        ("term" ~> "cx'" ~>
          Eithers.map
            ("tree" ~> lets [
              "flattenTree">: "t" ~>
                Lists.cons
                  (project PG._ElementTree PG._ElementTree_self @@ var "t")
                  (Lists.concat (Lists.map (var "flattenTree") (project PG._ElementTree PG._ElementTree_dependencies @@ var "t")))]
              $ var "flattenTree" @@ var "tree")
            (Util.coderEncode (Util.adapterCoder $ var "adapter") @@ var "cx'" @@ var "term")))

-- | Get all elements from a lazy graph
lazyGraphToElements :: TBinding (PG.LazyGraph v -> [PG.Element v])
lazyGraphToElements = define "lazyGraphToElements" $
  doc "Get all elements from a lazy graph" $
  "lg" ~>
    Lists.concat2
      (Lists.map ("x" ~> inject PG._Element PG._Element_vertex (var "x")) (project PG._LazyGraph PG._LazyGraph_vertices @@ var "lg"))
      (Lists.map ("x" ~> inject PG._Element PG._Element_edge (var "x")) (project PG._LazyGraph PG._LazyGraph_edges @@ var "lg"))

-- | Convert a property graph element to JSON
pgElementToJson :: TBinding (PGM.Schema Graph t v -> PG.Element v -> Context -> Either (InContext Error) JM.Value)
pgElementToJson = define "pgElementToJson" $
  doc "Convert a property graph element to JSON" $
  "schema" ~> "el" ~> "cx" ~>
    match PG._Element Nothing [
      PG._Element_vertex>>: "vertex" ~>
        Eithers.bind (Util.coderDecode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ var "cx" @@ (project PG._Vertex PG._Vertex_id @@ var "vertex"))
          ("term" ~> lets [
            "labelJson">: Json.valueString (unwrap PG._VertexLabel @@ (project PG._Vertex PG._Vertex_label @@ var "vertex"))] $
            Eithers.map
              ("propsJson" ~>
                Json.valueObject (Maps.fromList $ Maybes.cat $ list [
                  just (pair (string "label") (var "labelJson")),
                  just (pair (string "id") (Json.valueString $ ShowCore.term @@ var "term")),
                  var "propsJson"]))
              (propsToJson @@ var "schema" @@ var "cx" @@ (project PG._Vertex PG._Vertex_properties @@ var "vertex"))),
      PG._Element_edge>>: "edge" ~>
        Eithers.bind (Util.coderDecode (project PGM._Schema PGM._Schema_edgeIds @@ var "schema") @@ var "cx" @@ (project PG._Edge PG._Edge_id @@ var "edge"))
          ("term" ~> Eithers.bind (Util.coderDecode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ var "cx" @@ (project PG._Edge PG._Edge_out @@ var "edge"))
            ("termOut" ~> Eithers.bind (Util.coderDecode (project PGM._Schema PGM._Schema_vertexIds @@ var "schema") @@ var "cx" @@ (project PG._Edge PG._Edge_in @@ var "edge"))
              ("termIn" ~> lets [
                "labelJson">: Json.valueString (unwrap PG._EdgeLabel @@ (project PG._Edge PG._Edge_label @@ var "edge"))] $
                Eithers.map
                  ("propsJson" ~>
                    Json.valueObject (Maps.fromList $ Maybes.cat $ list [
                      just (pair (string "label") (var "labelJson")),
                      just (pair (string "id") (Json.valueString $ ShowCore.term @@ var "term")),
                      just (pair (string "out") (Json.valueString $ ShowCore.term @@ var "termOut")),
                      just (pair (string "in") (Json.valueString $ ShowCore.term @@ var "termIn")),
                      var "propsJson"]))
                  (propsToJson @@ var "schema" @@ var "cx" @@ (project PG._Edge PG._Edge_properties @@ var "edge")))))]
    @@ var "el"

-- | Convert a list of property graph elements to JSON
pgElementsToJson :: TBinding (PGM.Schema Graph t v -> [PG.Element v] -> Context -> Either (InContext Error) JM.Value)
pgElementsToJson = define "pgElementsToJson" $
  doc "Convert a list of property graph elements to JSON" $
  "schema" ~> "els" ~> "cx" ~>
    Eithers.map ("els'" ~> Json.valueArray (var "els'")) (Eithers.mapList ("el" ~> pgElementToJson @@ var "schema" @@ var "el" @@ var "cx") (var "els"))

-- Internal helper (not exported as a binding)
propsToJson :: TTerm (PGM.Schema Graph t v -> Context -> M.Map PG.PropertyKey v -> Either (InContext Error) (Y.Maybe (String, JM.Value)))
propsToJson = "schema" ~> "cx" ~> "pairs" ~>
  Logic.ifElse (Maps.null $ var "pairs")
    (right nothing)
    (Eithers.map
      ("p" ~> just (pair (string "properties") (Json.valueObject $ Maps.fromList $ var "p")))
      (Eithers.mapList
        ("pair" ~> lets [
          "key">: Pairs.first $ var "pair",
          "v">: Pairs.second $ var "pair"] $
          Eithers.bind (Util.coderDecode (project PGM._Schema PGM._Schema_propertyValues @@ var "schema") @@ var "cx" @@ var "v")
            ("term" ~> right (pair (unwrap PG._PropertyKey @@ var "key") (Json.valueString $ ShowCore.term @@ var "term"))))
        (Maps.toList $ var "pairs")))
