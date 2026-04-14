-- Note: this is an automatically generated file. Do not edit.

-- | Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs

module Hydra.Pg.Rdf.Mappings where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Pg.Model as PgModel
import qualified Hydra.Pg.Rdf.Environment as Environment
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Rdf.Utils as Utils
import qualified Hydra.Shacl.Model as ShaclModel
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Convert edge types into property shape constraints for a given vertex label
edgeTypesToPropertyShapes :: t0 -> (PgModel.EdgeLabel -> Syntax.Iri) -> PgModel.VertexLabel -> [PgModel.EdgeType t1] -> [ShaclModel.CommonConstraint]
edgeTypesToPropertyShapes encodeVertexLabel encodeEdgeLabel vertexLabel edgeTypes =
    Lists.concat (Lists.map (\et ->
      let outLabel = PgModel.edgeTypeOut et
          matchesVertex = Equality.equal (PgModel.unVertexLabel outLabel) (PgModel.unVertexLabel vertexLabel)
          edgeShape =
                  ShaclModel.CommonConstraintProperty (Sets.singleton (ShaclModel.ReferenceAnonymous (ShaclModel.PropertyShape {
                    ShaclModel.propertyShapeCommon = ShaclModel.CommonProperties {
                      ShaclModel.commonPropertiesConstraints = (Sets.singleton (ShaclModel.CommonConstraintClass (Sets.singleton (Syntax.RdfsClass ())))),
                      ShaclModel.commonPropertiesDeactivated = Nothing,
                      ShaclModel.commonPropertiesMessage = Utils.emptyLangStrings,
                      ShaclModel.commonPropertiesSeverity = ShaclModel.SeverityViolation,
                      ShaclModel.commonPropertiesTargetClass = Sets.empty,
                      ShaclModel.commonPropertiesTargetNode = Sets.empty,
                      ShaclModel.commonPropertiesTargetObjectsOf = Sets.empty,
                      ShaclModel.commonPropertiesTargetSubjectsOf = Sets.empty},
                    ShaclModel.propertyShapeConstraints = Sets.empty,
                    ShaclModel.propertyShapeDefaultValue = Nothing,
                    ShaclModel.propertyShapeDescription = Utils.emptyLangStrings,
                    ShaclModel.propertyShapeName = Utils.emptyLangStrings,
                    ShaclModel.propertyShapeOrder = Nothing,
                    ShaclModel.propertyShapePath = (encodeEdgeLabel (PgModel.edgeTypeLabel et))})))
      in (Logic.ifElse matchesVertex [
        edgeShape] [])) edgeTypes)

-- | Encode a property graph edge as an RDF description
encodeEdge :: Environment.PgRdfEnvironment t0 -> PgModel.Edge t0 -> Syntax.Description
encodeEdge env edge =

      let elab = PgModel.edgeLabel edge
          eout = PgModel.edgeOut edge
          ein = PgModel.edgeIn edge
          subj = Syntax.ResourceIri (Environment.pgRdfEnvironmentEncodeVertexId env eout)
          obj = Syntax.NodeIri (Environment.pgRdfEnvironmentEncodeVertexId env ein)
          pred = Environment.pgRdfEnvironmentEncodeEdgeLabel env elab
      in Syntax.Description {
        Syntax.descriptionSubject = (Utils.resourceToNode subj),
        Syntax.descriptionGraph = (Syntax.Graph (Sets.singleton (Syntax.Triple {
          Syntax.tripleSubject = subj,
          Syntax.triplePredicate = pred,
          Syntax.tripleObject = obj})))}

-- | Encode a lazy property graph as an RDF graph
encodeLazyGraph :: Environment.PgRdfEnvironment t0 -> PgModel.LazyGraph t0 -> Syntax.Graph
encodeLazyGraph env lg =

      let vertexDescs = Lists.map (encodeVertex env) (PgModel.lazyGraphVertices lg)
          edgeDescs = Lists.map (encodeEdge env) (PgModel.lazyGraphEdges lg)
          allDescs =
                  Lists.concat [
                    vertexDescs,
                    edgeDescs]
      in (Utils.descriptionsToGraph allDescs)

-- | Encode a property graph vertex as an RDF description
encodeVertex :: Environment.PgRdfEnvironment t0 -> PgModel.Vertex t0 -> Syntax.Description
encodeVertex env vertex =

      let vlab = PgModel.vertexLabel vertex
          vid = PgModel.vertexId vertex
          vprops = PgModel.vertexProperties vertex
          subj = Syntax.ResourceIri (Environment.pgRdfEnvironmentEncodeVertexId env vid)
          rtype = Syntax.NodeIri (Environment.pgRdfEnvironmentEncodeVertexLabel env vlab)
          typeTriple =
                  Syntax.Triple {
                    Syntax.tripleSubject = subj,
                    Syntax.triplePredicate = (Utils.rdfIri "type"),
                    Syntax.tripleObject = rtype}
          propTriples =
                  Lists.map (\kv ->
                    let key = Pairs.first kv
                        val = Pairs.second kv
                        pred = Environment.pgRdfEnvironmentEncodePropertyKey env key
                        obj = Syntax.NodeLiteral (Environment.pgRdfEnvironmentEncodePropertyValue env val)
                    in Syntax.Triple {
                      Syntax.tripleSubject = subj,
                      Syntax.triplePredicate = pred,
                      Syntax.tripleObject = obj}) (Maps.toList vprops)
          allTriples = Lists.cons typeTriple propTriples
      in Syntax.Description {
        Syntax.descriptionSubject = (Utils.resourceToNode subj),
        Syntax.descriptionGraph = (Syntax.Graph (Sets.fromList allTriples))}

-- | Convert a property graph schema to a SHACL shapes graph
graphSchemaToShapesGraph :: (t0 -> Syntax.Iri) -> (PgModel.VertexLabel -> Syntax.Iri) -> (PgModel.EdgeLabel -> Syntax.Iri) -> (PgModel.PropertyKey -> Syntax.Iri) -> PgModel.GraphSchema t0 -> ShaclModel.ShapesGraph
graphSchemaToShapesGraph encodeType encodeVertexLabel encodeEdgeLabel encodeKey schema =

      let vertexTypes = Maps.elems (PgModel.graphSchemaVertices schema)
          edgeTypes = Maps.elems (PgModel.graphSchemaEdges schema)
          defs =
                  Lists.map (\vt ->
                    let baseDef = vertexTypeToNodeShape encodeType encodeVertexLabel encodeKey vt
                        edgeShapes = edgeTypesToPropertyShapes encodeVertexLabel encodeEdgeLabel (PgModel.vertexTypeLabel vt) edgeTypes
                        baseShape = ShaclModel.definitionTarget baseDef
                        baseNode =
                                case baseShape of
                                  ShaclModel.ShapeNode v0 -> v0
                                  ShaclModel.ShapeProperty _ -> ShaclModel.NodeShape {
                                    ShaclModel.nodeShapeCommon = ShaclModel.CommonProperties {
                                      ShaclModel.commonPropertiesConstraints = Sets.empty,
                                      ShaclModel.commonPropertiesDeactivated = Nothing,
                                      ShaclModel.commonPropertiesMessage = Utils.emptyLangStrings,
                                      ShaclModel.commonPropertiesSeverity = ShaclModel.SeverityViolation,
                                      ShaclModel.commonPropertiesTargetClass = Sets.empty,
                                      ShaclModel.commonPropertiesTargetNode = Sets.empty,
                                      ShaclModel.commonPropertiesTargetObjectsOf = Sets.empty,
                                      ShaclModel.commonPropertiesTargetSubjectsOf = Sets.empty}}
                        baseCommon = ShaclModel.nodeShapeCommon baseNode
                        mergedConstraints = Sets.union (ShaclModel.commonPropertiesConstraints baseCommon) (Sets.fromList edgeShapes)
                        updatedCommon =
                                ShaclModel.CommonProperties {
                                  ShaclModel.commonPropertiesConstraints = mergedConstraints,
                                  ShaclModel.commonPropertiesDeactivated = (ShaclModel.commonPropertiesDeactivated baseCommon),
                                  ShaclModel.commonPropertiesMessage = (ShaclModel.commonPropertiesMessage baseCommon),
                                  ShaclModel.commonPropertiesSeverity = (ShaclModel.commonPropertiesSeverity baseCommon),
                                  ShaclModel.commonPropertiesTargetClass = (ShaclModel.commonPropertiesTargetClass baseCommon),
                                  ShaclModel.commonPropertiesTargetNode = (ShaclModel.commonPropertiesTargetNode baseCommon),
                                  ShaclModel.commonPropertiesTargetObjectsOf = (ShaclModel.commonPropertiesTargetObjectsOf baseCommon),
                                  ShaclModel.commonPropertiesTargetSubjectsOf = (ShaclModel.commonPropertiesTargetSubjectsOf baseCommon)}
                        updatedShape = ShaclModel.ShapeNode (ShaclModel.NodeShape {
                              ShaclModel.nodeShapeCommon = updatedCommon})
                    in ShaclModel.Definition {
                      ShaclModel.definitionIri = (ShaclModel.definitionIri baseDef),
                      ShaclModel.definitionTarget = updatedShape}) vertexTypes
      in (ShaclModel.ShapesGraph (Sets.fromList defs))

-- | Convert a property type to a SHACL property shape
propertyTypeToPropertyShape :: (t0 -> Syntax.Iri) -> (PgModel.PropertyKey -> Syntax.Iri) -> PgModel.PropertyType t0 -> ShaclModel.PropertyShape
propertyTypeToPropertyShape encodeType encodeKey pt =

      let key = PgModel.propertyTypeKey pt
          path = encodeKey key
          required_ = PgModel.propertyTypeRequired pt
          dtIri = encodeType (PgModel.propertyTypeValue pt)
          constraints = Sets.singleton (ShaclModel.CommonConstraintDatatype dtIri)
          propConstraints = Logic.ifElse required_ (Sets.singleton (ShaclModel.PropertyShapeConstraintMinCount 1)) Sets.empty
      in ShaclModel.PropertyShape {
        ShaclModel.propertyShapeCommon = ShaclModel.CommonProperties {
          ShaclModel.commonPropertiesConstraints = constraints,
          ShaclModel.commonPropertiesDeactivated = Nothing,
          ShaclModel.commonPropertiesMessage = Utils.emptyLangStrings,
          ShaclModel.commonPropertiesSeverity = ShaclModel.SeverityViolation,
          ShaclModel.commonPropertiesTargetClass = Sets.empty,
          ShaclModel.commonPropertiesTargetNode = Sets.empty,
          ShaclModel.commonPropertiesTargetObjectsOf = Sets.empty,
          ShaclModel.commonPropertiesTargetSubjectsOf = Sets.empty},
        ShaclModel.propertyShapeConstraints = propConstraints,
        ShaclModel.propertyShapeDefaultValue = Nothing,
        ShaclModel.propertyShapeDescription = Utils.emptyLangStrings,
        ShaclModel.propertyShapeName = Utils.emptyLangStrings,
        ShaclModel.propertyShapeOrder = Nothing,
        ShaclModel.propertyShapePath = path}

-- | Convert a vertex type to a SHACL node shape definition
vertexTypeToNodeShape :: (t0 -> Syntax.Iri) -> (PgModel.VertexLabel -> Syntax.Iri) -> (PgModel.PropertyKey -> Syntax.Iri) -> PgModel.VertexType t0 -> ShaclModel.Definition ShaclModel.Shape
vertexTypeToNodeShape encodeType encodeLabel encodeKey vt =

      let label = PgModel.vertexTypeLabel vt
          labelIri = encodeLabel label
          propTypes = PgModel.vertexTypeProperties vt
          propShapes =
                  Lists.map (\pt -> ShaclModel.CommonConstraintProperty (Sets.singleton (ShaclModel.ReferenceAnonymous (propertyTypeToPropertyShape encodeType encodeKey pt)))) propTypes
          common =
                  ShaclModel.CommonProperties {
                    ShaclModel.commonPropertiesConstraints = (Sets.fromList propShapes),
                    ShaclModel.commonPropertiesDeactivated = Nothing,
                    ShaclModel.commonPropertiesMessage = Utils.emptyLangStrings,
                    ShaclModel.commonPropertiesSeverity = ShaclModel.SeverityViolation,
                    ShaclModel.commonPropertiesTargetClass = (Sets.singleton (Syntax.RdfsClass ())),
                    ShaclModel.commonPropertiesTargetNode = Sets.empty,
                    ShaclModel.commonPropertiesTargetObjectsOf = Sets.empty,
                    ShaclModel.commonPropertiesTargetSubjectsOf = Sets.empty}
      in ShaclModel.Definition {
        ShaclModel.definitionIri = labelIri,
        ShaclModel.definitionTarget = (ShaclModel.ShapeNode (ShaclModel.NodeShape {
          ShaclModel.nodeShapeCommon = common}))}
