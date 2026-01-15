module Hydra.Ext.Sources.Pg.Graphson.Coder where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  adjacentEdgeToJson, doubleValueToJson, edgeMapToJson, edgePropertyMapToJson,
  floatValueToJson, mapToJson, toJsonObject, typedValueToJson, valueToJson,
  vertexPropertyMapToJson, vertexPropertyValueToJson, vertexToJson)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Meta.Ast           as Ast
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
import qualified Hydra.Dsl.Meta.Lib.Strings   as Strings
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
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Ext.Sources.Pg.Graphson.Syntax as GraphsonSyntax
import qualified Hydra.Sources.Json.Model as JsonModel
import qualified Hydra.Pg.Graphson.Syntax as G  -- Generated phantom types
import qualified Hydra.Json.Model as JM         -- Generated JSON types
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.pg.graphson.coder"

module_ :: Module
module_ = Module ns elements
    []  -- term dependencies (none needed - we use primitive operations)
    (kernelTypesNamespaces L.++ [GraphsonSyntax.ns, JsonModel.ns]) $  -- type dependencies
    Just "Encoding functions for converting GraphSON syntax to JSON."
  where
    elements = [
      toBinding adjacentEdgeToJson,
      toBinding doubleValueToJson,
      toBinding edgeMapToJson,
      toBinding edgePropertyMapToJson,
      toBinding floatValueToJson,
      toBinding mapToJson,
      toBinding toJsonObject,
      toBinding typedValueToJson,
      toBinding valueToJson,
      toBinding vertexPropertyMapToJson,
      toBinding vertexPropertyValueToJson,
      toBinding vertexToJson]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

-- Type references
gson :: String -> Type
gson = Bootstrap.typeref GraphsonSyntax.ns

jsonValue :: Type
jsonValue = Bootstrap.typeref JsonModel.ns "Value"

-- | Convert a DoubleValue to JSON
doubleValueToJson :: TBinding (G.DoubleValue -> JM.Value)
doubleValueToJson = define "doubleValueToJson" $
  doc "Convert a GraphSON DoubleValue to a JSON Value" $
  match G._DoubleValue Nothing [
    G._DoubleValue_finite>>: "d" ~> Json.valueNumber (Literals.float64ToBigfloat $ var "d"),
    G._DoubleValue_infinity>>: constant $ Json.valueString (string "Infinity"),
    G._DoubleValue_negativeInfinity>>: constant $ Json.valueString (string "-Infinity"),
    G._DoubleValue_notANumber>>: constant $ Json.valueString (string "NaN")]

-- | Convert a FloatValue to JSON
floatValueToJson :: TBinding (G.FloatValue -> JM.Value)
floatValueToJson = define "floatValueToJson" $
  doc "Convert a GraphSON FloatValue to a JSON Value" $
  match G._FloatValue Nothing [
    G._FloatValue_finite>>: "f" ~> Json.valueNumber (Literals.float32ToBigfloat $ var "f"),
    G._FloatValue_infinity>>: constant $ Json.valueString (string "Infinity"),
    G._FloatValue_negativeInfinity>>: constant $ Json.valueString (string "-Infinity"),
    G._FloatValue_notANumber>>: constant $ Json.valueString (string "NaN")]

-- | Create a typed JSON object with @type and @value fields
typedValueToJson :: TBinding (String -> JM.Value -> JM.Value)
typedValueToJson = define "typedValueToJson" $
  doc "Create a typed JSON object with @type and @value fields" $
  "typeName" ~> "valueJson" ~>
    toJsonObject @@ list [
      pair (string "@type") (just $ Json.valueString $ var "typeName"),
      pair (string "@value") (just $ var "valueJson")]

-- | Create a JSON object from a list of key-value pairs, filtering out Nothing values
toJsonObject :: TBinding ([(String, Maybe JM.Value)] -> JM.Value)
toJsonObject = define "toJsonObject" $
  doc "Create a JSON object from a list of key-value pairs, filtering out Nothing values" $
  "pairs" ~>
    Json.valueObject $ Maps.fromList $ Maybes.cat $ Lists.map
      ("p" ~> Maybes.map
        ("v" ~> pair (Pairs.first $ var "p") (var "v"))
        (Pairs.second $ var "p"))
      (var "pairs")

-- | Convert a GraphSON Map to JSON
mapToJson :: TBinding (G.Map -> JM.Value)
mapToJson = define "mapToJson" $
  doc "Convert a GraphSON Map to a JSON array of alternating keys and values" $
  "m" ~>
    Json.valueArray $ Lists.concat $ Lists.map
      ("vp" ~> list [
        valueToJson @@ (project G._ValuePair G._ValuePair_first @@ var "vp"),
        valueToJson @@ (project G._ValuePair G._ValuePair_second @@ var "vp")])
      (unwrap G._Map @@ var "m")

-- | Convert a GraphSON Value to JSON
valueToJson :: TBinding (G.Value -> JM.Value)
valueToJson = define "valueToJson" $
  doc "Convert a GraphSON Value to a JSON Value" $
  match G._Value Nothing [
    G._Value_bigDecimal>>: "bd" ~>
      typedValueToJson @@ string "g:BigDecimal" @@ (Json.valueString $ unwrap G._BigDecimalValue @@ var "bd"),
    G._Value_bigInteger>>: "i" ~>
      typedValueToJson @@ string "g:BigInteger" @@ (Json.valueNumber $ Literals.bigintToBigfloat $ var "i"),
    -- Note: binary is represented as base64-encoded string in GraphSON
    G._Value_binary>>: "b" ~>
      typedValueToJson @@ string "g:Binary" @@ (Json.valueString $ var "b"),
    G._Value_boolean>>: "b" ~> Json.valueBoolean (var "b"),
    G._Value_byte>>: "b" ~>
      typedValueToJson @@ string "g:Byte" @@ (Json.valueNumber $ Literals.bigintToBigfloat $ Literals.uint8ToBigint $ var "b"),
    G._Value_char>>: "c" ~>
      typedValueToJson @@ string "g:Char" @@ (Json.valueString $ Strings.fromList $ Lists.pure $ Literals.bigintToInt32 $ Literals.uint32ToBigint $ var "c"),
    G._Value_composite>>: "ctv" ~>
      typedValueToJson
        @@ (unwrap G._TypeName @@ (project G._CompositeTypedValue G._CompositeTypedValue_type @@ var "ctv"))
        @@ (mapToJson @@ (project G._CompositeTypedValue G._CompositeTypedValue_fields @@ var "ctv")),
    G._Value_dateTime>>: "dt" ~>
      typedValueToJson @@ string "g:DateTime" @@ (Json.valueString $ unwrap G._DateTime @@ var "dt"),
    G._Value_double>>: "dv" ~>
      typedValueToJson @@ string "g:Double" @@ (doubleValueToJson @@ var "dv"),
    G._Value_duration>>: "dur" ~>
      typedValueToJson @@ string "g:Duration" @@ (Json.valueString $ unwrap G._Duration @@ var "dur"),
    G._Value_float>>: "fv" ~>
      typedValueToJson @@ string "g:Float" @@ (floatValueToJson @@ var "fv"),
    G._Value_integer>>: "i" ~>
      typedValueToJson @@ string "g:Int32" @@ (Json.valueNumber $ Literals.bigintToBigfloat $ Literals.int32ToBigint $ var "i"),
    G._Value_list>>: "vals" ~>
      typedValueToJson @@ string "g:List" @@ (Json.valueArray $ Lists.map valueToJson $ var "vals"),
    G._Value_long>>: "l" ~>
      typedValueToJson @@ string "g:Long" @@ (Json.valueNumber $ Literals.bigintToBigfloat $ Literals.int64ToBigint $ var "l"),
    G._Value_map>>: "m" ~>
      typedValueToJson @@ string "g:Map" @@ (mapToJson @@ var "m"),
    G._Value_null>>: constant Json.valueNull,
    G._Value_primitive>>: "ptv" ~>
      typedValueToJson @@ string "g:PrimitivePdt" @@ (Json.valueString $ project G._PrimitiveTypedValue G._PrimitiveTypedValue_value @@ var "ptv"),
    G._Value_set>>: "vals" ~>
      typedValueToJson @@ string "g:Set" @@ (Json.valueArray $ Lists.map valueToJson $ var "vals"),
    G._Value_short>>: "i" ~>
      typedValueToJson @@ string "g:Int16" @@ (Json.valueNumber $ Literals.bigintToBigfloat $ Literals.int16ToBigint $ var "i"),
    G._Value_string>>: "s" ~> Json.valueString (var "s"),
    G._Value_uuid>>: "u" ~>
      typedValueToJson @@ string "g:UUID" @@ (Json.valueString $ unwrap G._Uuid @@ var "u")]

-- | Convert a VertexPropertyValue to JSON
vertexPropertyValueToJson :: TBinding (G.VertexPropertyValue -> JM.Value)
vertexPropertyValueToJson = define "vertexPropertyValueToJson" $
  doc "Convert a GraphSON VertexPropertyValue to a JSON Value" $
  "vpv" ~>
    toJsonObject @@ list [
      pair (string "id") (Maybes.map valueToJson $ project G._VertexPropertyValue G._VertexPropertyValue_id @@ var "vpv"),
      pair (string "value") (just $ valueToJson @@ (project G._VertexPropertyValue G._VertexPropertyValue_value @@ var "vpv"))]

-- | Convert an AdjacentEdge to JSON
adjacentEdgeToJson :: TBinding (Bool -> G.AdjacentEdge -> JM.Value)
adjacentEdgeToJson = define "adjacentEdgeToJson" $
  doc "Convert a GraphSON AdjacentEdge to a JSON Value. The Bool indicates whether this is an outgoing edge." $
  "out" ~> "ae" ~>
    toJsonObject @@ list [
      pair (string "id") (just $ valueToJson @@ (project G._AdjacentEdge G._AdjacentEdge_id @@ var "ae")),
      pair (string "inV") (Logic.ifElse (var "out")
        (just $ valueToJson @@ (project G._AdjacentEdge G._AdjacentEdge_vertexId @@ var "ae"))
        nothing),
      pair (string "outV") (Logic.ifElse (var "out")
        nothing
        (just $ valueToJson @@ (project G._AdjacentEdge G._AdjacentEdge_vertexId @@ var "ae"))),
      pair (string "properties") (edgePropertyMapToJson @@ (project G._AdjacentEdge G._AdjacentEdge_properties @@ var "ae"))]

-- | Convert edge properties map to JSON
edgePropertyMapToJson :: TBinding (M.Map G.PropertyKey G.Value -> Maybe JM.Value)
edgePropertyMapToJson = define "edgePropertyMapToJson" $
  doc "Convert a map of edge properties to an optional JSON Value" $
  "m" ~>
    Logic.ifElse (Maps.null $ var "m")
      nothing
      (just $ Json.valueObject $ Maps.fromList $ Lists.map
        ("p" ~> pair
          (unwrap G._PropertyKey @@ (Pairs.first $ var "p"))
          (valueToJson @@ (Pairs.second $ var "p")))
        (Maps.toList $ var "m"))

-- | Convert edge map to JSON
edgeMapToJson :: TBinding (Bool -> M.Map G.EdgeLabel [G.AdjacentEdge] -> Maybe JM.Value)
edgeMapToJson = define "edgeMapToJson" $
  doc "Convert a map of edges by label to an optional JSON Value" $
  "out" ~> "m" ~>
    Logic.ifElse (Maps.null $ var "m")
      nothing
      (just $ Json.valueObject $ Maps.fromList $ Lists.map
        ("p" ~> pair
          (unwrap G._EdgeLabel @@ (Pairs.first $ var "p"))
          (Json.valueArray $ Lists.map (adjacentEdgeToJson @@ var "out") (Pairs.second $ var "p")))
        (Maps.toList $ var "m"))

-- | Convert vertex properties map to JSON
vertexPropertyMapToJson :: TBinding (M.Map G.PropertyKey [G.VertexPropertyValue] -> Maybe JM.Value)
vertexPropertyMapToJson = define "vertexPropertyMapToJson" $
  doc "Convert a map of vertex properties to an optional JSON Value" $
  "m" ~>
    Logic.ifElse (Maps.null $ var "m")
      nothing
      (just $ Json.valueObject $ Maps.fromList $ Lists.map
        ("p" ~> pair
          (unwrap G._PropertyKey @@ (Pairs.first $ var "p"))
          (Json.valueArray $ Lists.map vertexPropertyValueToJson (Pairs.second $ var "p")))
        (Maps.toList $ var "m"))

-- | Convert a GraphSON Vertex to JSON
vertexToJson :: TBinding (G.Vertex -> JM.Value)
vertexToJson = define "vertexToJson" $
  doc "Convert a GraphSON Vertex to a JSON Value" $
  "v" ~>
    toJsonObject @@ list [
      pair (string "id") (just $ valueToJson @@ (project G._Vertex G._Vertex_id @@ var "v")),
      pair (string "label") (Maybes.map
        ("lbl" ~> Json.valueString $ unwrap G._VertexLabel @@ var "lbl")
        (project G._Vertex G._Vertex_label @@ var "v")),
      pair (string "inE") (edgeMapToJson @@ boolean False @@ (project G._Vertex G._Vertex_inEdges @@ var "v")),
      pair (string "outE") (edgeMapToJson @@ boolean True @@ (project G._Vertex G._Vertex_outEdges @@ var "v")),
      pair (string "properties") (vertexPropertyMapToJson @@ (project G._Vertex G._Vertex_properties @@ var "v"))]
