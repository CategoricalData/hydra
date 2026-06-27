{-# LANGUAGE ScopedTypeVariables #-}

-- | The TinkerPop "Modern" property graph, encoded as Hydra terms.
--
-- This is a translingual term module: 'modernSchema' and 'modernGraph' are
-- 'DefinitionTerm's built from the phantom term DSL, so they generate to all
-- host languages (unlike the genpg demo's Hydra.Dsl.Pg.Schemas helpers, which
-- produce plain Haskell values).
--
-- The schema is a hydra.pg.model.GraphSchema parameterized by
-- hydra.core.LiteralType (id/property types); the data is a
-- hydra.pg.model.Graph parameterized by hydra.core.Literal (id/property values).
--
-- Content is the canonical TinkerPop Modern graph (HydraPop ExampleGraphs.java).

module Hydra.Sources.Tinkerpop.Examples.Modern where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms        as Phantoms
import qualified Hydra.Overlay.Haskell.Bootstrap                  as Bootstrap
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core            as Core
import qualified Hydra.Pg.Model                                   as PG
import qualified Hydra.Sources.Pg.Model                          as PgModel
import qualified Hydra.Sources.Kernel.Types.Core                 as KernelCore
import qualified Data.Map                                        as M


ns :: ModuleName
ns = ModuleName "hydra.tinkerpop.examples.modern"

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

module_ :: Module
module_ = Module {
    moduleName = ns,
    moduleDefinitions = [
      Phantoms.toDefinition modernGraph,
      Phantoms.toDefinition modernSchema],
    moduleDependencies = Bootstrap.unqualifiedDep <$> [KernelCore.ns, PgModel.ns],
    moduleMetadata = Bootstrap.descriptionMetadata $ Just
      "The TinkerPop \"Modern\" property graph (schema and data), encoded as Hydra terms. The canonical example graph used throughout Apache TinkerPop documentation."}

-- Helpers --------------------------------------------------------------------

-- | A vertex/edge label as a wrapped string.
vlabel :: String -> TypedTerm PG.VertexLabel
vlabel s = Phantoms.wrap PG._VertexLabel (Phantoms.string s)

elabel :: String -> TypedTerm PG.EdgeLabel
elabel s = Phantoms.wrap PG._EdgeLabel (Phantoms.string s)

-- | A property key as a wrapped string.
pkey :: String -> TypedTerm PG.PropertyKey
pkey s = Phantoms.wrap PG._PropertyKey (Phantoms.string s)

-- | A LiteralType term (used as the schema's "t" type parameter).
ltString :: TypedTerm LiteralType
ltString = Core.literalTypeString

ltInt32 :: TypedTerm LiteralType
ltInt32 = Core.literalTypeInteger Core.integerTypeInt32

ltFloat64 :: TypedTerm LiteralType
ltFloat64 = Core.literalTypeFloat Core.floatTypeFloat64

-- | A Literal term (used as the graph's "v" value/id parameter).
lvString :: String -> TypedTerm Literal
lvString s = Core.literalString (Phantoms.string s)

lvInt32 :: Int -> TypedTerm Literal
lvInt32 i = Core.literalInteger (Core.integerValueInt32 (Phantoms.int32 i))

lvFloat64 :: Double -> TypedTerm Literal
lvFloat64 d = Core.literalFloat (Core.floatValueFloat64 (Phantoms.float64 d))

-- | A PropertyType (schema-level property declaration).
mkPropertyType :: String -> TypedTerm LiteralType -> Bool -> TypedTerm (PG.PropertyType LiteralType)
mkPropertyType k v req = Phantoms.record PG._PropertyType [
  PG._PropertyType_key>>: pkey k,
  PG._PropertyType_value>>: v,
  PG._PropertyType_required>>: Phantoms.boolean req]

-- | A VertexType keyed by its label, for the schema's vertex map.
mkVertexType :: String -> [TypedTerm (PG.PropertyType LiteralType)]
  -> (TypedTerm PG.VertexLabel, TypedTerm (PG.VertexType LiteralType))
mkVertexType lbl props = (
  vlabel lbl,
  Phantoms.record PG._VertexType [
    PG._VertexType_label>>: vlabel lbl,
    PG._VertexType_id>>: ltInt32,
    PG._VertexType_properties>>: Phantoms.list props])

-- | An EdgeType keyed by its label, for the schema's edge map.
mkEdgeType :: String -> String -> String -> [TypedTerm (PG.PropertyType LiteralType)]
  -> (TypedTerm PG.EdgeLabel, TypedTerm (PG.EdgeType LiteralType))
mkEdgeType lbl outLbl inLbl props = (
  elabel lbl,
  Phantoms.record PG._EdgeType [
    PG._EdgeType_label>>: elabel lbl,
    PG._EdgeType_id>>: ltInt32,
    PG._EdgeType_out>>: vlabel outLbl,
    PG._EdgeType_in>>: vlabel inLbl,
    PG._EdgeType_properties>>: Phantoms.list props])

-- | A Vertex keyed by its id, for the graph's vertex map.
vertex :: Int -> String -> [(String, TypedTerm Literal)]
  -> (TypedTerm Literal, TypedTerm (PG.Vertex Literal))
vertex vid lbl props = (
  lvInt32 vid,
  Phantoms.record PG._Vertex [
    PG._Vertex_label>>: vlabel lbl,
    PG._Vertex_id>>: lvInt32 vid,
    PG._Vertex_properties>>: propertyMap props])

-- | An Edge keyed by its id, for the graph's edge map.
edge :: Int -> String -> Int -> Int -> Double
  -> (TypedTerm Literal, TypedTerm (PG.Edge Literal))
edge eid lbl outV inV weight = (
  lvInt32 eid,
  Phantoms.record PG._Edge [
    PG._Edge_label>>: elabel lbl,
    PG._Edge_id>>: lvInt32 eid,
    PG._Edge_out>>: lvInt32 outV,
    PG._Edge_in>>: lvInt32 inV,
    PG._Edge_properties>>: propertyMap [("weight", lvFloat64 weight)]])

-- | A property map (PropertyKey -> Literal) for a vertex or edge.
propertyMap :: [(String, TypedTerm Literal)] -> TypedTerm (M.Map PG.PropertyKey Literal)
propertyMap kvs = Phantoms.map $ M.fromList $ fmap (\(k, v) -> (pkey k, v)) kvs

-- Definitions (alphabetical) -------------------------------------------------

modernGraph :: TypedTermDefinition (PG.Graph Literal)
modernGraph = define "modernGraph" $
  Phantoms.doc "The TinkerPop Modern property graph data, with int32 Literal ids" $
  Phantoms.record PG._Graph [
    PG._Graph_vertices>>: (Phantoms.map $ M.fromList [
      vertex 1 "person"   [("name", lvString "marko"),  ("age", lvInt32 29)],
      vertex 2 "person"   [("name", lvString "vadas"),  ("age", lvInt32 27)],
      vertex 3 "software" [("name", lvString "lop"),    ("lang", lvString "java")],
      vertex 4 "person"   [("name", lvString "josh"),   ("age", lvInt32 32)],
      vertex 5 "software" [("name", lvString "ripple"), ("lang", lvString "java")],
      vertex 6 "person"   [("name", lvString "peter"),  ("age", lvInt32 35)]]),
    PG._Graph_edges>>: (Phantoms.map $ M.fromList [
      edge  7 "knows"   1 2 0.5,
      edge  8 "knows"   1 4 1.0,
      edge  9 "created" 1 3 0.4,
      edge 10 "created" 4 5 1.0,
      edge 11 "created" 4 3 0.4,
      edge 12 "created" 6 3 0.2])]

modernSchema :: TypedTermDefinition (PG.GraphSchema LiteralType)
modernSchema = define "modernSchema" $
  Phantoms.doc "The TinkerPop Modern property graph schema, with LiteralType property/id types" $
  Phantoms.record PG._GraphSchema [
    PG._GraphSchema_vertices>>: (Phantoms.map $ M.fromList [
      mkVertexType "person" [
        mkPropertyType "name" ltString True,
        mkPropertyType "age"  ltInt32  False],
      mkVertexType "software" [
        mkPropertyType "name" ltString True,
        mkPropertyType "lang" ltString True]]),
    PG._GraphSchema_edges>>: (Phantoms.map $ M.fromList [
      mkEdgeType "knows"   "person" "person" [
        mkPropertyType "weight" ltFloat64 True],
      mkEdgeType "created" "person" "software" [
        mkPropertyType "weight" ltFloat64 True]])]
