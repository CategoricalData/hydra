-- Note: this is an automatically generated file. Do not edit.
-- | A mapping between Hydra's property-graph model (hydra.pg.model, TinkerPop-shaped) and the Neo4j model (hydra.neo4j.model). It is not an isomorphism and not invertible: ids and values are bridged by caller-supplied conversions (Neo4jMapping); a Neo4j relationship type expands into a property-graph edge label conditionally on the schema; and multi-label nodes, which Hydra's single-label vertices cannot represent, are rejected.

module Hydra.Neo4j.Pg where
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Overlay.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Overlay.Haskell.Lib.Equality as Equality
import qualified Hydra.Overlay.Haskell.Lib.Lists as Lists
import qualified Hydra.Overlay.Haskell.Lib.Logic as Logic
import qualified Hydra.Overlay.Haskell.Lib.Maps as Maps
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Overlay.Haskell.Lib.Sets as Sets
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Neo4j.Model as Neo4jModel
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Compute the property-graph edge label for a Neo4j relationship type, given the schema and endpoint labels.
edgeLabelForRelationship :: Neo4jModel.GraphType -> Neo4jModel.NodeLabel -> Neo4jModel.NodeLabel -> Neo4jModel.RelationshipType -> PgModel.EdgeLabel
edgeLabelForRelationship gt startLabel endLabel relType =

      let typeStr = Neo4jModel.unRelationshipType relType
          matching =
                  Lists.filter (\ret -> Equality.equal (Neo4jModel.unRelationshipType (Neo4jModel.relationshipElementTypeType ret)) typeStr) (Neo4jModel.graphTypeRelationships gt)
          camelType = Formatting.convertCase Util.CaseConventionUpperSnake Util.CaseConventionCamel typeStr
          startStr = Neo4jModel.unNodeLabel startLabel
          endStr = Neo4jModel.unNodeLabel endLabel
          expanded =
                  Strings.cat [
                    Formatting.decapitalize (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionCamel startStr),
                    (Formatting.capitalize camelType),
                    (Formatting.capitalize (Formatting.convertCase Util.CaseConventionPascal Util.CaseConventionCamel endStr))]
      in (PgModel.EdgeLabel (Logic.ifElse (Equality.gt (Lists.length matching) 1) expanded camelType))
-- | Convert a property-graph edge to a Neo4j relationship (edge label -> UPPER_SNAKE type).
edgeToRelationship :: Neo4jModel.Neo4jMapping t0 -> PgModel.Edge t0 -> Either String Neo4jModel.Relationship
edgeToRelationship mapping edge =
    Eithers.bind (Neo4jModel.neo4jMappingEncodeId mapping (PgModel.edgeId edge)) (\eid -> Eithers.bind (Neo4jModel.neo4jMappingEncodeId mapping (PgModel.edgeOut edge)) (\startId -> Eithers.bind (Neo4jModel.neo4jMappingEncodeId mapping (PgModel.edgeIn edge)) (\endId -> Eithers.bind ((\mapping2 -> \props -> Eithers.map (\pairs -> Maps.fromList pairs) (Eithers.mapList (\entry -> Eithers.map (\encoded -> (Neo4jModel.Key (PgModel.unPropertyKey (Pairs.first entry)), encoded)) (Neo4jModel.neo4jMappingEncodeValue mapping2 (Pairs.second entry))) (Maps.toList props))) mapping (PgModel.edgeProperties edge)) (\props -> Right (Neo4jModel.Relationship {
      Neo4jModel.relationshipId = eid,
      Neo4jModel.relationshipProperties = props,
      Neo4jModel.relationshipType = (Neo4jModel.RelationshipType (Formatting.convertCase Util.CaseConventionCamel Util.CaseConventionUpperSnake (PgModel.unEdgeLabel (PgModel.edgeLabel edge)))),
      Neo4jModel.relationshipStart = startId,
      Neo4jModel.relationshipEnd = endId})))))
-- | Convert a whole property-graph Graph to a Neo4j graph (nodes and relationships).
graphToNeo4j :: Ord t0 => (Neo4jModel.Neo4jMapping t0 -> PgModel.Graph t0 -> Either String ([Neo4jModel.Node], [Neo4jModel.Relationship]))
graphToNeo4j mapping graph =
    Eithers.bind (Eithers.mapList (\v -> vertexToNode mapping v) (Maps.elems (PgModel.graphVertices graph))) (\nodes -> Eithers.bind (Eithers.mapList (\e -> edgeToRelationship mapping e) (Maps.elems (PgModel.graphEdges graph))) (\rels -> Right (nodes, rels)))
-- | Convert a Neo4j graph (nodes and relationships) to property-graph vertices and edges against a GraphType.
neo4jToGraph :: Neo4jModel.Neo4jMapping t0 -> Neo4jModel.GraphType -> [Neo4jModel.Node] -> [Neo4jModel.Relationship] -> Either String ([PgModel.Vertex t0], [PgModel.Edge t0])
neo4jToGraph mapping gt nodes rels =

      let labelOf =
              (\nodes2 -> \eid ->
                let m =
                        Maps.fromList (Lists.map (\n -> (Neo4jModel.unElementId (Neo4jModel.nodeId n), (Sets.toList (Neo4jModel.nodeLabels n)))) nodes2)
                in (Optionals.cases (Maps.lookup (Neo4jModel.unElementId eid) m) (Left (Strings.cat [
                  "no node with id ",
                  (Neo4jModel.unElementId eid)])) (\labels -> Logic.ifElse (Equality.equal (Lists.length labels) 1) (Right (Optionals.fromOptional (Neo4jModel.NodeLabel "") (Lists.maybeHead labels))) (Left (Strings.cat [
                  "endpoint node is not single-labeled: ",
                  (Neo4jModel.unElementId eid)]))))) nodes
      in (Eithers.bind (Eithers.mapList (\n -> nodeToVertex mapping n) nodes) (\vertices -> Eithers.bind (Eithers.mapList (\r -> Eithers.bind (labelOf (Neo4jModel.relationshipStart r)) (\startLabel -> Eithers.bind (labelOf (Neo4jModel.relationshipEnd r)) (\endLabel -> relationshipToEdge mapping gt startLabel endLabel r))) rels) (\edges -> Right (vertices, edges))))
-- | Convert a Neo4j node to a property-graph vertex; fails on a multi-label node.
nodeToVertex :: Neo4jModel.Neo4jMapping t0 -> Neo4jModel.Node -> Either String (PgModel.Vertex t0)
nodeToVertex mapping node =

      let labels = Sets.toList (Neo4jModel.nodeLabels node)
          soleLabel = Optionals.fromOptional (Neo4jModel.NodeLabel "") (Lists.maybeHead labels)
      in (Logic.ifElse (Equality.equal (Lists.length labels) 1) (Eithers.bind (Neo4jModel.neo4jMappingDecodeId mapping (Neo4jModel.nodeId node)) (\vid -> Eithers.bind ((\mapping2 -> \props -> Eithers.map (\pairs -> Maps.fromList pairs) (Eithers.mapList (\entry -> Eithers.map (\decoded -> (PgModel.PropertyKey (Neo4jModel.unKey (Pairs.first entry)), decoded)) (Neo4jModel.neo4jMappingDecodeValue mapping2 (Pairs.second entry))) (Maps.toList props))) mapping (Neo4jModel.nodeProperties node)) (\props -> Right (PgModel.Vertex {
        PgModel.vertexLabel = (PgModel.VertexLabel (Neo4jModel.unNodeLabel soleLabel)),
        PgModel.vertexId = vid,
        PgModel.vertexProperties = props})))) (Left (Strings.cat [
        "cannot map a multi-label Neo4j node to a single-label property-graph vertex: ",
        (Neo4jModel.unElementId (Neo4jModel.nodeId node))])))
-- | Convert a Neo4j relationship to a property-graph edge, expanding the type via the schema and endpoint labels.
relationshipToEdge :: Neo4jModel.Neo4jMapping t0 -> Neo4jModel.GraphType -> Neo4jModel.NodeLabel -> Neo4jModel.NodeLabel -> Neo4jModel.Relationship -> Either String (PgModel.Edge t0)
relationshipToEdge mapping gt startLabel endLabel rel =
    Eithers.bind (Neo4jModel.neo4jMappingDecodeId mapping (Neo4jModel.relationshipId rel)) (\eid -> Eithers.bind (Neo4jModel.neo4jMappingDecodeId mapping (Neo4jModel.relationshipStart rel)) (\outId -> Eithers.bind (Neo4jModel.neo4jMappingDecodeId mapping (Neo4jModel.relationshipEnd rel)) (\inId -> Eithers.bind ((\mapping2 -> \props -> Eithers.map (\pairs -> Maps.fromList pairs) (Eithers.mapList (\entry -> Eithers.map (\decoded -> (PgModel.PropertyKey (Neo4jModel.unKey (Pairs.first entry)), decoded)) (Neo4jModel.neo4jMappingDecodeValue mapping2 (Pairs.second entry))) (Maps.toList props))) mapping (Neo4jModel.relationshipProperties rel)) (\props -> Right (PgModel.Edge {
      PgModel.edgeLabel = (edgeLabelForRelationship gt startLabel endLabel (Neo4jModel.relationshipType rel)),
      PgModel.edgeId = eid,
      PgModel.edgeOut = outId,
      PgModel.edgeIn = inId,
      PgModel.edgeProperties = props})))))
-- | Convert a property-graph vertex to a Neo4j node (single label -> singleton label set).
vertexToNode :: Neo4jModel.Neo4jMapping t0 -> PgModel.Vertex t0 -> Either String Neo4jModel.Node
vertexToNode mapping vertex =
    Eithers.bind (Neo4jModel.neo4jMappingEncodeId mapping (PgModel.vertexId vertex)) (\eid -> Eithers.bind ((\mapping2 -> \props -> Eithers.map (\pairs -> Maps.fromList pairs) (Eithers.mapList (\entry -> Eithers.map (\encoded -> (Neo4jModel.Key (PgModel.unPropertyKey (Pairs.first entry)), encoded)) (Neo4jModel.neo4jMappingEncodeValue mapping2 (Pairs.second entry))) (Maps.toList props))) mapping (PgModel.vertexProperties vertex)) (\props -> Right (Neo4jModel.Node {
      Neo4jModel.nodeId = eid,
      Neo4jModel.nodeLabels = (Sets.singleton (Neo4jModel.NodeLabel (PgModel.unVertexLabel (PgModel.vertexLabel vertex)))),
      Neo4jModel.nodeProperties = props})))
