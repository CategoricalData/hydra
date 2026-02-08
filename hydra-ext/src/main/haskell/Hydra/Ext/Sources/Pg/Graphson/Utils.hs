module Hydra.Ext.Sources.Pg.Graphson.Utils where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  elementsToVerticesWithAdjacentEdges, encodeStringValue, encodeTermValue,
  pgElementsToGraphson)
import Hydra.Sources.Libraries
import           Hydra.Dsl.Meta.Lib.Strings                as Strings
import           Hydra.Dsl.Meta.Phantoms                   as Phantoms
import qualified Hydra.Dsl.Annotations                     as Annotations
import qualified Hydra.Dsl.Bootstrap                       as Bootstrap
import qualified Hydra.Dsl.Grammars                        as Grammars
import qualified Hydra.Dsl.LiteralTypes                    as LiteralTypes
import qualified Hydra.Dsl.Literals                        as Literals
import qualified Hydra.Dsl.Meta.Accessors                  as Accessors
import qualified Hydra.Dsl.Meta.Ast                        as Ast
import qualified Hydra.Dsl.Meta.Base                       as MetaBase
import qualified Hydra.Dsl.Meta.Coders                     as Coders
import qualified Hydra.Dsl.Meta.Compute                    as Compute
import qualified Hydra.Dsl.Meta.Core                       as Core
import qualified Hydra.Dsl.Meta.Grammar                    as Grammar
import qualified Hydra.Dsl.Meta.Graph                      as Graph
import qualified Hydra.Dsl.Meta.Json                       as Json
import qualified Hydra.Dsl.Meta.Lib.Chars                  as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality               as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows                  as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals               as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Meta.Lib.Math                   as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes                 as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Meta.Module                     as Module
import qualified Hydra.Dsl.Meta.Terms                      as MetaTerms
import qualified Hydra.Dsl.Meta.Testing                    as Testing
import qualified Hydra.Dsl.Meta.Topology                   as Topology
import qualified Hydra.Dsl.Meta.Types                      as MetaTypes
import qualified Hydra.Dsl.Meta.Typing                     as Typing
import qualified Hydra.Dsl.Meta.Util                       as Util
import qualified Hydra.Dsl.Meta.Variants                   as Variants
import qualified Hydra.Dsl.Prims                           as Prims
import qualified Hydra.Dsl.Tabular                         as Tabular
import qualified Hydra.Dsl.Terms                           as Terms
import qualified Hydra.Dsl.Tests                           as Tests
import qualified Hydra.Dsl.Types                           as Types
import qualified Hydra.Sources.Decode.Core                 as DecodeCore
import qualified Hydra.Sources.Encode.Core                 as EncodeCore
import qualified Hydra.Sources.Kernel.Terms.Adapt.Literals as AdaptLiterals
import qualified Hydra.Sources.Kernel.Terms.Adapt.Modules  as AdaptModules
import qualified Hydra.Sources.Kernel.Terms.Adapt.Simple   as AdaptSimple
import qualified Hydra.Sources.Kernel.Terms.Adapt.Terms    as AdaptTerms
import qualified Hydra.Sources.Kernel.Terms.Adapt.Utils    as AdaptUtils
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
import qualified Hydra.Sources.Kernel.Terms.Monads         as Monads
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
import qualified Hydra.Sources.Kernel.Terms.Tarjan         as Tarjan
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
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Ext.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Ext.Sources.Pg.Graphson.Construct as GraphsonConstruct
import qualified Hydra.Ext.Sources.Pg.Model as PgModel
import qualified Hydra.Sources.Json.Model as JsonModel
import qualified Hydra.Pg.Graphson.Syntax as G  -- Generated phantom types
import qualified Hydra.Pg.Model as PG            -- Generated PG model types
import qualified Hydra.Json.Model as JM          -- Generated JSON types


ns :: Namespace
ns = Namespace "hydra.pg.graphson.utils"

module_ :: Module
module_ = Module ns elements
    [GraphsonConstruct.ns, Rewriting.ns]  -- term dependencies
    (kernelTypesNamespaces L.++ [GraphsonSyntax.ns, PgModel.ns, JsonModel.ns]) $  -- type dependencies
    Just "Utility functions for GraphSON encoding and property graph conversion."
  where
    elements = [
      toBinding elementsToVerticesWithAdjacentEdges,
      toBinding encodeStringValue,
      toBinding encodeTermValue,
      toBinding pgElementsToGraphson]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Type references
gson :: String -> Type
gson = Bootstrap.typeref GraphsonSyntax.ns

pg :: String -> Type
pg = Bootstrap.typeref PgModel.ns

jsonValue :: Type
jsonValue = Bootstrap.typeref JsonModel.ns "Value"

-- | Encode a String value to GraphSON
encodeStringValue :: TBinding (String -> Flow s G.Value)
encodeStringValue = define "encodeStringValue" $
  doc "Encode a String value as a GraphSON Value" $
  "s" ~>
    Flows.pure $ inject G._Value G._Value_string (var "s")

-- | Encode a Term value to GraphSON
encodeTermValue :: TBinding (Term -> Flow s G.Value)
encodeTermValue = define "encodeTermValue" $
  doc "Encode a Hydra Term as a GraphSON Value. Supports literals and unit values." $
  "term" ~>
    match _Term (Just $ Flows.fail $ string "unsupported term variant for GraphSON encoding") [
      _Term_literal>>: "lit" ~>
        match _Literal (Just $ Flows.fail $ string "unsupported literal type for GraphSON encoding") [
          _Literal_binary>>: "b" ~>
            Flows.pure $ inject G._Value G._Value_binary (Literals.binaryToString $ var "b"),
          _Literal_boolean>>: "b" ~>
            Flows.pure $ inject G._Value G._Value_boolean (var "b"),
          _Literal_float>>: "fv" ~>
            match _FloatValue (Just $ Flows.fail $ string "unsupported float type") [
              _FloatValue_bigfloat>>: "f" ~>
                Flows.pure $ inject G._Value G._Value_bigDecimal
                  (wrap G._BigDecimalValue $ Literals.showBigfloat $ var "f"),
              _FloatValue_float32>>: "f" ~>
                Flows.pure $ inject G._Value G._Value_float
                  (inject G._FloatValue G._FloatValue_finite (var "f")),
              _FloatValue_float64>>: "f" ~>
                Flows.pure $ inject G._Value G._Value_double
                  (inject G._DoubleValue G._DoubleValue_finite (var "f"))]
            @@ var "fv",
          _Literal_integer>>: "iv" ~>
            match _IntegerValue (Just $ Flows.fail $ string "unsupported integer type") [
              _IntegerValue_bigint>>: "i" ~>
                Flows.pure $ inject G._Value G._Value_bigInteger (var "i"),
              _IntegerValue_int32>>: "i" ~>
                Flows.pure $ inject G._Value G._Value_integer (var "i"),
              _IntegerValue_int64>>: "i" ~>
                Flows.pure $ inject G._Value G._Value_long (var "i")]
            @@ var "iv",
          _Literal_string>>: "s" ~>
            Flows.pure $ inject G._Value G._Value_string (var "s")]
        @@ var "lit",
      _Term_unit>>: constant $
        Flows.pure $ injectUnit G._Value G._Value_null]
    @@ (Rewriting.deannotateTerm @@ var "term")

-- | Convert a list of PG elements to vertices with adjacent edges
elementsToVerticesWithAdjacentEdges :: TBinding ([PG.Element v] -> [PG.VertexWithAdjacentEdges v])
elementsToVerticesWithAdjacentEdges = define "elementsToVerticesWithAdjacentEdges" $
  doc "Convert a list of property graph elements to a list of vertices with their adjacent edges" $
  "els" ~>
    -- Partition elements into vertices and edges
    "partitioned" <~ (Lists.foldl
      ("acc" ~> "el" ~>
        match PG._Element Nothing [
          PG._Element_vertex>>: "v" ~>
            pair
              (Lists.cons (var "v") (Pairs.first $ var "acc"))
              (Pairs.second $ var "acc"),
          PG._Element_edge>>: "e" ~>
            pair
              (Pairs.first $ var "acc")
              (Lists.cons (var "e") (Pairs.second $ var "acc"))]
        @@ var "el")
      (pair (list ([] :: [TTerm (PG.Vertex v)])) (list ([] :: [TTerm (PG.Edge v)])))
      (var "els")) $
    -- Note: foldl with cons reverses the list, so we reverse back to preserve input order
    "vertices" <~ (Lists.reverse $ Pairs.first $ var "partitioned") $
    "edges" <~ (Lists.reverse $ Pairs.second $ var "partitioned") $
    -- Build initial vertex map (vertex id -> VertexWithAdjacentEdges with empty edge lists)
    "vertexMap0" <~ (Maps.fromList $ Lists.map
      ("v" ~>
        pair
          (project PG._Vertex PG._Vertex_id @@ var "v")
          (record PG._VertexWithAdjacentEdges [
            PG._VertexWithAdjacentEdges_vertex>>: var "v",
            PG._VertexWithAdjacentEdges_ins>>: list ([] :: [TTerm (PG.AdjacentEdge v)]),
            PG._VertexWithAdjacentEdges_outs>>: list ([] :: [TTerm (PG.AdjacentEdge v)])]))
      (var "vertices")) $
    -- Add edges to the vertex map
    "vertexMap1" <~ (Lists.foldl
      ("vmap" ~> "edge" ~>
        "label" <~ (project PG._Edge PG._Edge_label @@ var "edge") $
        "edgeId" <~ (project PG._Edge PG._Edge_id @@ var "edge") $
        "outV" <~ (project PG._Edge PG._Edge_out @@ var "edge") $
        "inV" <~ (project PG._Edge PG._Edge_in @@ var "edge") $
        "props" <~ (project PG._Edge PG._Edge_properties @@ var "edge") $
        -- Create adjacent edge for out-vertex (points to in-vertex)
        "adjEdgeOut" <~ (record PG._AdjacentEdge [
          PG._AdjacentEdge_label>>: var "label",
          PG._AdjacentEdge_id>>: var "edgeId",
          PG._AdjacentEdge_vertex>>: var "inV",
          PG._AdjacentEdge_properties>>: var "props"]) $
        -- Create adjacent edge for in-vertex (points to out-vertex)
        "adjEdgeIn" <~ (record PG._AdjacentEdge [
          PG._AdjacentEdge_label>>: var "label",
          PG._AdjacentEdge_id>>: var "edgeId",
          PG._AdjacentEdge_vertex>>: var "outV",
          PG._AdjacentEdge_properties>>: var "props"]) $
        -- Add to out-vertex's outs list
        "vmap1" <~ (Maybes.maybe
          (var "vmap")
          ("vae" ~>
            Maps.insert (var "outV")
              (record PG._VertexWithAdjacentEdges [
                PG._VertexWithAdjacentEdges_vertex>>: project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_vertex @@ var "vae",
                PG._VertexWithAdjacentEdges_ins>>: project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_ins @@ var "vae",
                PG._VertexWithAdjacentEdges_outs>>: Lists.cons (var "adjEdgeOut") (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_outs @@ var "vae")])
              (var "vmap"))
          (Maps.lookup (var "outV") (var "vmap"))) $
        -- Add to in-vertex's ins list
        Maybes.maybe
          (var "vmap1")
          ("vae" ~>
            Maps.insert (var "inV")
              (record PG._VertexWithAdjacentEdges [
                PG._VertexWithAdjacentEdges_vertex>>: project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_vertex @@ var "vae",
                PG._VertexWithAdjacentEdges_ins>>: Lists.cons (var "adjEdgeIn") (project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_ins @@ var "vae"),
                PG._VertexWithAdjacentEdges_outs>>: project PG._VertexWithAdjacentEdges PG._VertexWithAdjacentEdges_outs @@ var "vae"])
              (var "vmap1"))
          (Maps.lookup (var "inV") (var "vmap1")))
      (var "vertexMap0")
      (var "edges")) $
    Maps.elems (var "vertexMap1")

-- | Convert PG elements to GraphSON JSON values
pgElementsToGraphson :: TBinding ((v -> Flow s G.Value) -> [PG.Element v] -> Flow s [JM.Value])
pgElementsToGraphson = define "pgElementsToGraphson" $
  doc "Convert property graph elements to a list of GraphSON JSON values" $
  "encodeValue" ~> "els" ~>
    Flows.mapList
      (GraphsonConstruct.pgVertexWithAdjacentEdgesToJson @@ var "encodeValue")
      (elementsToVerticesWithAdjacentEdges @@ var "els")
