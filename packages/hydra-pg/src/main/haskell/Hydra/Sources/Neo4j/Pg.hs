{-# LANGUAGE ScopedTypeVariables #-}
module Hydra.Sources.Neo4j.Pg where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel hiding (
  Node(..), _Node, Relationship(..), _Relationship, Path(..), _Path,
  Element(..), _Element)
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import qualified Hydra.Dsl.Lib.Eithers                as Eithers
import qualified Hydra.Dsl.Lib.Equality               as Equality
import qualified Hydra.Dsl.Lib.Lists                  as Lists
import qualified Hydra.Dsl.Lib.Logic                  as Logic
import qualified Hydra.Dsl.Lib.Maps                   as Maps
import qualified Hydra.Dsl.Lib.Optionals                 as Optionals
import qualified Hydra.Dsl.Lib.Pairs                  as Pairs
import qualified Hydra.Dsl.Lib.Sets                   as Sets
import qualified Hydra.Dsl.Lib.Strings                as Strings
import qualified Hydra.Dsl.Util                       as Util
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms                   as Phantoms
import           Prelude hiding ((++))
import qualified Data.List                                 as L
import qualified Data.Map                                  as M
import qualified Data.Set                                  as S

-- Additional imports
import Hydra.Neo4j.Model as N4
import qualified Hydra.Pg.Model                            as PG
import qualified Hydra.Sources.Neo4j.Model                 as Neo4jModel
import qualified Hydra.Sources.Kernel.Terms.Formatting     as Formatting


ns :: ModuleName
ns = ModuleName "hydra.neo4j.pg"

mappingDefinition :: String -> TypedTerm a -> TypedTermDefinition a
mappingDefinition = definitionInModule module_

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [Neo4jModel.ns, ModuleName "hydra.pg.model", ModuleName "hydra.core", Formatting.ns, ModuleName "hydra.util"],
            moduleMetadata = descriptionMetadata (Just "A mapping between Hydra's property-graph model (hydra.pg.model, TinkerPop-shaped) and the Neo4j model (hydra.neo4j.model). It is not an isomorphism and not invertible: ids and values are bridged by caller-supplied conversions (Neo4jMapping); a Neo4j relationship type expands into a property-graph edge label conditionally on the schema; and multi-label nodes, which Hydra's single-label vertices cannot represent, are rejected.")}
  where
   definitions = [
     toDefinition edgeLabelForRelationship,
     toDefinition edgeToRelationship,
     toDefinition (graphToNeo4j :: TypedTermDefinition (N4.Neo4jMapping Int -> PG.Graph Int -> Either String ([N4.Node], [Relationship]))),
     toDefinition neo4jToGraph,
     toDefinition nodeToVertex,
     toDefinition relationshipToEdge,
     toDefinition vertexToNode]

-- ============================================================================
-- Edge label <-> relationship type (mismatch 3: overloading + case)
-- ============================================================================

-- | Compute the property-graph edge label for a Neo4j relationship, given the
-- graph type and the (single) start and end labels of its endpoints. If the
-- relationship's type has a single RelationshipElementType in the schema, the
-- label is simply the type recased from UPPER_SNAKE to camelCase (LIKES ->
-- likes). If the type is overloaded (more than one element type), the label is
-- disambiguated by the endpoint labels: LIKES from a Person to a Movie becomes
-- personLikesMovie.
edgeLabelForRelationship :: TypedTermDefinition (GraphType -> NodeLabel -> NodeLabel -> RelationshipType -> PG.EdgeLabel)
edgeLabelForRelationship = mappingDefinition "edgeLabelForRelationship" $
  doc "Compute the property-graph edge label for a Neo4j relationship type, given the schema and endpoint labels." $
  "gt" ~> "startLabel" ~> "endLabel" ~> "relType" ~>
  lets [
    "typeStr">: unwrap _RelationshipType @@ var "relType",
    -- Element types in the schema whose relationship type equals this one.
    "matching">: (Lists.filter
      ("ret" ~> Equality.equal
        (unwrap _RelationshipType @@ (project _RelationshipElementType _RelationshipElementType_type @@ var "ret"))
        (var "typeStr"))
      (project _GraphType _GraphType_relationships @@ var "gt")),
    "camelType">: Formatting.convertCase @@ Util.caseConventionUpperSnake @@ Util.caseConventionCamel @@ var "typeStr",
    "startStr">: unwrap _NodeLabel @@ var "startLabel",
    "endStr">: unwrap _NodeLabel @@ var "endLabel",
    -- Overloaded: disambiguate with the endpoint labels (out ++ Type ++ In).
    "expanded">: Strings.cat (list [
      Formatting.decapitalize @@ (Formatting.convertCase @@ Util.caseConventionPascal @@ Util.caseConventionCamel @@ var "startStr"),
      Formatting.capitalize @@ var "camelType",
      Formatting.capitalize @@ (Formatting.convertCase @@ Util.caseConventionPascal @@ Util.caseConventionCamel @@ var "endStr")])]
  $ wrap PG._EdgeLabel (Logic.ifElse
      (Equality.gt (Lists.length (var "matching")) (int32 1))
      (var "expanded")
      (var "camelType"))

-- ============================================================================
-- Vertex <-> Node
-- ============================================================================

-- | Convert a property-graph vertex to a Neo4j node. The single vertex label
-- becomes a singleton Neo4j label set; the id and each property value are
-- converted via the caller-supplied encoders, short-circuiting on failure.
vertexToNode :: TypedTermDefinition (N4.Neo4jMapping v -> PG.Vertex v -> Either String N4.Node)
vertexToNode = mappingDefinition "vertexToNode" $
  doc "Convert a property-graph vertex to a Neo4j node (single label -> singleton label set)." $
  "mapping" ~> "vertex" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_encodeId @@ var "mapping") @@ (project PG._Vertex PG._Vertex_id @@ var "vertex"))
    ("eid" ~>
  Eithers.bind (encodeProperties @@ var "mapping" @@ (project PG._Vertex PG._Vertex_properties @@ var "vertex"))
    ("props" ~>
    right (record _Node [
      _Node_id>>: var "eid",
      _Node_labels>>: Sets.singleton (wrap _NodeLabel (unwrap PG._VertexLabel @@ (project PG._Vertex PG._Vertex_label @@ var "vertex")) :: TypedTerm NodeLabel),
      _Node_properties>>: var "props"])))

-- | Convert a Neo4j node to a property-graph vertex. Fails if the node has more
-- than one label, which Hydra's single-label vertices cannot represent (the
-- expressiveness gap). The id and property values are decoded via the caller's
-- decoders.
nodeToVertex :: TypedTermDefinition (N4.Neo4jMapping v -> N4.Node -> Either String (PG.Vertex v))
nodeToVertex = mappingDefinition "nodeToVertex" $
  doc "Convert a Neo4j node to a property-graph vertex; fails on a multi-label node." $
  "mapping" ~> "node" ~>
  lets [
    "labels">: Sets.toList (project _Node _Node_labels @@ var "node" :: TypedTerm (S.Set NodeLabel)),
    "soleLabel">: Optionals.fromOptional (wrap _NodeLabel (string "")) (Lists.maybeHead (var "labels"))]
  $ Logic.ifElse
      (Equality.equal (Lists.length (var "labels")) (int32 1))
      (Eithers.bind ((project _Neo4jMapping _Neo4jMapping_decodeId @@ var "mapping") @@ (project _Node _Node_id @@ var "node"))
        ("vid" ~>
       Eithers.bind (decodeProperties @@ var "mapping" @@ (project _Node _Node_properties @@ var "node"))
        ("props" ~>
        right (record PG._Vertex [
          PG._Vertex_label>>: wrap PG._VertexLabel (unwrap _NodeLabel @@ var "soleLabel"),
          PG._Vertex_id>>: var "vid",
          PG._Vertex_properties>>: var "props"]))))
      (left (Strings.cat (list [
        string "cannot map a multi-label Neo4j node to a single-label property-graph vertex: ",
        unwrap _ElementId @@ (project _Node _Node_id @@ var "node")])))

-- ============================================================================
-- Edge <-> Relationship
-- ============================================================================

-- | Convert a property-graph edge to a Neo4j relationship. The edge label is
-- recased from camelCase to UPPER_SNAKE (likes -> LIKES); no un-expansion of an
-- overloaded label is attempted, so this is deliberately not the inverse of
-- relationshipToEdge. The id, endpoints, and property values use the caller's
-- encoders.
edgeToRelationship :: TypedTermDefinition (N4.Neo4jMapping v -> PG.Edge v -> Either String Relationship)
edgeToRelationship = mappingDefinition "edgeToRelationship" $
  doc "Convert a property-graph edge to a Neo4j relationship (edge label -> UPPER_SNAKE type)." $
  "mapping" ~> "edge" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_encodeId @@ var "mapping") @@ (project PG._Edge PG._Edge_id @@ var "edge"))
    ("eid" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_encodeId @@ var "mapping") @@ (project PG._Edge PG._Edge_out @@ var "edge"))
    ("startId" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_encodeId @@ var "mapping") @@ (project PG._Edge PG._Edge_in @@ var "edge"))
    ("endId" ~>
  Eithers.bind (encodeProperties @@ var "mapping" @@ (project PG._Edge PG._Edge_properties @@ var "edge"))
    ("props" ~>
    right (record _Relationship [
      _Relationship_id>>: var "eid",
      _Relationship_properties>>: var "props",
      _Relationship_type>>: wrap _RelationshipType (Formatting.convertCase @@ Util.caseConventionCamel @@ Util.caseConventionUpperSnake @@ (unwrap PG._EdgeLabel @@ (project PG._Edge PG._Edge_label @@ var "edge"))),
      _Relationship_start>>: var "startId",
      _Relationship_end>>: var "endId"])))))

-- | Convert a Neo4j relationship to a property-graph edge, given the graph type
-- (to decide single vs. overloaded type) and the start/end node labels (to
-- disambiguate an overloaded type). The id, endpoints, and property values use
-- the caller's decoders.
relationshipToEdge :: TypedTermDefinition (N4.Neo4jMapping v -> GraphType -> NodeLabel -> NodeLabel -> Relationship -> Either String (PG.Edge v))
relationshipToEdge = mappingDefinition "relationshipToEdge" $
  doc "Convert a Neo4j relationship to a property-graph edge, expanding the type via the schema and endpoint labels." $
  "mapping" ~> "gt" ~> "startLabel" ~> "endLabel" ~> "rel" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_decodeId @@ var "mapping") @@ (project _Relationship _Relationship_id @@ var "rel"))
    ("eid" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_decodeId @@ var "mapping") @@ (project _Relationship _Relationship_start @@ var "rel"))
    ("outId" ~>
  Eithers.bind ((project _Neo4jMapping _Neo4jMapping_decodeId @@ var "mapping") @@ (project _Relationship _Relationship_end @@ var "rel"))
    ("inId" ~>
  Eithers.bind (decodeProperties @@ var "mapping" @@ (project _Relationship _Relationship_properties @@ var "rel"))
    ("props" ~>
    right (record PG._Edge [
      PG._Edge_label>>: edgeLabelForRelationship @@ var "gt" @@ var "startLabel" @@ var "endLabel" @@ (project _Relationship _Relationship_type @@ var "rel"),
      PG._Edge_id>>: var "eid",
      PG._Edge_out>>: var "outId",
      PG._Edge_in>>: var "inId",
      PG._Edge_properties>>: var "props"])))))

-- ============================================================================
-- Whole-graph wrappers
-- ============================================================================

-- | Convert a whole property-graph Graph to a Neo4j graph (a list of nodes and
-- a list of relationships). Each vertex and edge is converted via the per-
-- element functions, short-circuiting on the first failure. No GraphType is
-- needed: the PG -> Neo4j direction only recases edge labels.
graphToNeo4j :: forall v. Ord v => TypedTermDefinition (N4.Neo4jMapping v -> PG.Graph v -> Either String ([N4.Node], [Relationship]))
graphToNeo4j = mappingDefinition "graphToNeo4j" $
  doc "Convert a whole property-graph Graph to a Neo4j graph (nodes and relationships)." $
  "mapping" ~> "graph" ~>
  Eithers.bind (Eithers.mapList
      ("v" ~> vertexToNode @@ var "mapping" @@ var "v")
      (Maps.elems (project PG._Graph PG._Graph_vertices @@ var "graph" :: TypedTerm (M.Map v (PG.Vertex v)))))
    ("nodes" ~>
  Eithers.bind (Eithers.mapList
      ("e" ~> edgeToRelationship @@ var "mapping" @@ var "e")
      (Maps.elems (project PG._Graph PG._Graph_edges @@ var "graph" :: TypedTerm (M.Map v (PG.Edge v)))))
    ("rels" ~>
    right (pair (var "nodes") (var "rels"))))

-- | Convert a Neo4j graph (a list of nodes and a list of relationships) to
-- property-graph vertices and edges, against a GraphType. Each node is
-- converted via nodeToVertex (failing on multi-label nodes), and each
-- relationship via relationshipToEdge, whose edge-label expansion needs the
-- endpoint nodes' labels — resolved here from the node list (each node's single
-- label). Returns the vertices and edges as lists; the caller assembles them
-- into a Graph (its `v` provides the Ord instance the Graph's id-keyed maps
-- need, which this translingual function cannot impose on a polymorphic `v`).
neo4jToGraph :: TypedTermDefinition (N4.Neo4jMapping v -> GraphType -> [N4.Node] -> [Relationship] -> Either String ([PG.Vertex v], [PG.Edge v]))
neo4jToGraph = mappingDefinition "neo4jToGraph" $
  doc "Convert a Neo4j graph (nodes and relationships) to property-graph vertices and edges against a GraphType." $
  "mapping" ~> "gt" ~> "nodes" ~> "rels" ~>
  lets [
    "labelOf">: soleLabelForId @@ var "nodes"]
  $ Eithers.bind (Eithers.mapList
      ("n" ~> nodeToVertex @@ var "mapping" @@ var "n")
      (var "nodes"))
    ("vertices" ~>
  Eithers.bind (Eithers.mapList
      ("r" ~> Eithers.bind ((var "labelOf") @@ (project _Relationship _Relationship_start @@ var "r"))
        ("startLabel" ~> Eithers.bind ((var "labelOf") @@ (project _Relationship _Relationship_end @@ var "r"))
          ("endLabel" ~> relationshipToEdge @@ var "mapping" @@ var "gt" @@ var "startLabel" @@ var "endLabel" @@ var "r")))
      (var "rels"))
    ("edges" ~>
    right (pair (var "vertices") (var "edges"))))

-- | Build a resolver from element id to that id's single node label, for use in
-- neo4jToGraph's edge-label expansion. Returns Left if no node with that id is
-- present, or if the node carries other than exactly one label (which
-- nodeToVertex would itself reject).
soleLabelForId :: TypedTerm ([N4.Node] -> ElementId -> Either String NodeLabel)
soleLabelForId =
  "nodes" ~>
  lets [
    "m">: (Maps.fromList (Lists.map
      ("n" ~> pair
        (unwrap _ElementId @@ (project _Node _Node_id @@ var "n"))
        (Sets.toList (project _Node _Node_labels @@ var "n" :: TypedTerm (S.Set NodeLabel))))
      (var "nodes")) :: TypedTerm (M.Map String [NodeLabel]))]
  $ "eid" ~>
    Optionals.cases (Maps.lookup (unwrap _ElementId @@ var "eid" :: TypedTerm String) (var "m"))
      (left (Strings.cat (list [string "no node with id ", unwrap _ElementId @@ var "eid"])))
      ("labels" ~> Logic.ifElse
        (Equality.equal (Lists.length (var "labels")) (int32 1))
        (right (Optionals.fromOptional (wrap _NodeLabel (string "")) (Lists.maybeHead (var "labels"))))
        (left (Strings.cat (list [string "endpoint node is not single-labeled: ", unwrap _ElementId @@ var "eid"]))))

-- ============================================================================
-- Property maps (host helpers)
-- ============================================================================

-- | Encode a property-graph property map (PropertyKey -> v) to a Neo4j property
-- map (Key -> Value), applying the value encoder to each value and short-
-- circuiting on the first failure.
encodeProperties :: TypedTerm (N4.Neo4jMapping v -> M.Map PG.PropertyKey v -> Either String (M.Map Key N4.Value))
encodeProperties =
  "mapping" ~> "props" ~>
  Eithers.map (lambda "pairs" $ (Maps.fromList (var "pairs") :: TypedTerm (M.Map Key N4.Value)))
    (Eithers.mapList
      ("entry" ~>
        Eithers.map
          ("encoded" ~> pair (wrap _Key (unwrap PG._PropertyKey @@ (Pairs.first (var "entry")))) (var "encoded"))
          ((project _Neo4jMapping _Neo4jMapping_encodeValue @@ var "mapping") @@ (Pairs.second (var "entry"))))
      (Maps.toList (var "props" :: TypedTerm (M.Map PG.PropertyKey v))))

-- | Decode a Neo4j property map (Key -> Value) to a property-graph property map
-- (PropertyKey -> v), applying the value decoder to each value.
decodeProperties :: TypedTerm (N4.Neo4jMapping v -> M.Map Key N4.Value -> Either String (M.Map PG.PropertyKey v))
decodeProperties =
  "mapping" ~> "props" ~>
  Eithers.map (lambda "pairs" $ (Maps.fromList (var "pairs") :: TypedTerm (M.Map PG.PropertyKey v)))
    (Eithers.mapList
      ("entry" ~>
        Eithers.map
          ("decoded" ~> pair (wrap PG._PropertyKey (unwrap _Key @@ (Pairs.first (var "entry")))) (var "decoded"))
          ((project _Neo4jMapping _Neo4jMapping_decodeValue @@ var "mapping") @@ (Pairs.second (var "entry"))))
      (Maps.toList (var "props" :: TypedTerm (M.Map Key N4.Value))))
