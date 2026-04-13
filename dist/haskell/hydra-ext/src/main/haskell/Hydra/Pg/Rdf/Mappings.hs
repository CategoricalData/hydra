-- Note: this is an automatically generated file. Do not edit.

-- | Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs

module Hydra.Pg.Rdf.Mappings where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Pg.Rdf.Environment as Environment
import qualified Hydra.Rdf.Syntax as Syntax
import qualified Hydra.Rdf.Utils as Utils
import qualified Hydra.Shacl.Model as Model_
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Convert edge types into property shape constraints for a given vertex label
edgeTypesToPropertyShapes :: t0 -> (Model.EdgeLabel -> Syntax.Iri) -> Model.VertexLabel -> [Model.EdgeType t1] -> [Model_.CommonConstraint]
edgeTypesToPropertyShapes encodeVertexLabel encodeEdgeLabel vertexLabel edgeTypes =
    Lists.concat (Lists.map (\et ->
      let outLabel = Model.edgeTypeOut et
          matchesVertex = Equality.equal (Model.unVertexLabel outLabel) (Model.unVertexLabel vertexLabel)
          edgeShape =
                  Model_.CommonConstraintProperty (Sets.singleton (Model_.ReferenceAnonymous (Model_.PropertyShape {
                    Model_.propertyShapeCommon = Model_.CommonProperties {
                      Model_.commonPropertiesConstraints = (Sets.singleton (Model_.CommonConstraintClass (Sets.singleton (Syntax.RdfsClass ())))),
                      Model_.commonPropertiesDeactivated = Nothing,
                      Model_.commonPropertiesMessage = Utils.emptyLangStrings,
                      Model_.commonPropertiesSeverity = Model_.SeverityViolation,
                      Model_.commonPropertiesTargetClass = Sets.empty,
                      Model_.commonPropertiesTargetNode = Sets.empty,
                      Model_.commonPropertiesTargetObjectsOf = Sets.empty,
                      Model_.commonPropertiesTargetSubjectsOf = Sets.empty},
                    Model_.propertyShapeConstraints = Sets.empty,
                    Model_.propertyShapeDefaultValue = Nothing,
                    Model_.propertyShapeDescription = Utils.emptyLangStrings,
                    Model_.propertyShapeName = Utils.emptyLangStrings,
                    Model_.propertyShapeOrder = Nothing,
                    Model_.propertyShapePath = (encodeEdgeLabel (Model.edgeTypeLabel et))})))
      in (Logic.ifElse matchesVertex [
        edgeShape] [])) edgeTypes)

-- | Encode a property graph edge as an RDF description
encodeEdge :: Environment.PgRdfEnvironment t0 -> Model.Edge t0 -> Syntax.Description
encodeEdge env edge =

      let elab = Model.edgeLabel edge
          eout = Model.edgeOut edge
          ein = Model.edgeIn edge
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
encodeLazyGraph :: Environment.PgRdfEnvironment t0 -> Model.LazyGraph t0 -> Syntax.Graph
encodeLazyGraph env lg =

      let vertexDescs = Lists.map (encodeVertex env) (Model.lazyGraphVertices lg)
          edgeDescs = Lists.map (encodeEdge env) (Model.lazyGraphEdges lg)
          allDescs =
                  Lists.concat [
                    vertexDescs,
                    edgeDescs]
      in (Utils.descriptionsToGraph allDescs)

-- | Encode a property graph vertex as an RDF description
encodeVertex :: Environment.PgRdfEnvironment t0 -> Model.Vertex t0 -> Syntax.Description
encodeVertex env vertex =

      let vlab = Model.vertexLabel vertex
          vid = Model.vertexId vertex
          vprops = Model.vertexProperties vertex
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
graphSchemaToShapesGraph :: (t0 -> Syntax.Iri) -> (Model.VertexLabel -> Syntax.Iri) -> (Model.EdgeLabel -> Syntax.Iri) -> (Model.PropertyKey -> Syntax.Iri) -> Model.GraphSchema t0 -> Model_.ShapesGraph
graphSchemaToShapesGraph encodeType encodeVertexLabel encodeEdgeLabel encodeKey schema =

      let vertexTypes = Maps.elems (Model.graphSchemaVertices schema)
          edgeTypes = Maps.elems (Model.graphSchemaEdges schema)
          defs =
                  Lists.map (\vt ->
                    let baseDef = vertexTypeToNodeShape encodeType encodeVertexLabel encodeKey vt
                        edgeShapes = edgeTypesToPropertyShapes encodeVertexLabel encodeEdgeLabel (Model.vertexTypeLabel vt) edgeTypes
                        baseShape = Model_.definitionTarget baseDef
                        baseNode =
                                case baseShape of
                                  Model_.ShapeNode v0 -> v0
                                  Model_.ShapeProperty _ -> Model_.NodeShape {
                                    Model_.nodeShapeCommon = Model_.CommonProperties {
                                      Model_.commonPropertiesConstraints = Sets.empty,
                                      Model_.commonPropertiesDeactivated = Nothing,
                                      Model_.commonPropertiesMessage = Utils.emptyLangStrings,
                                      Model_.commonPropertiesSeverity = Model_.SeverityViolation,
                                      Model_.commonPropertiesTargetClass = Sets.empty,
                                      Model_.commonPropertiesTargetNode = Sets.empty,
                                      Model_.commonPropertiesTargetObjectsOf = Sets.empty,
                                      Model_.commonPropertiesTargetSubjectsOf = Sets.empty}}
                        baseCommon = Model_.nodeShapeCommon baseNode
                        mergedConstraints = Sets.union (Model_.commonPropertiesConstraints baseCommon) (Sets.fromList edgeShapes)
                        updatedCommon =
                                Model_.CommonProperties {
                                  Model_.commonPropertiesConstraints = mergedConstraints,
                                  Model_.commonPropertiesDeactivated = (Model_.commonPropertiesDeactivated baseCommon),
                                  Model_.commonPropertiesMessage = (Model_.commonPropertiesMessage baseCommon),
                                  Model_.commonPropertiesSeverity = (Model_.commonPropertiesSeverity baseCommon),
                                  Model_.commonPropertiesTargetClass = (Model_.commonPropertiesTargetClass baseCommon),
                                  Model_.commonPropertiesTargetNode = (Model_.commonPropertiesTargetNode baseCommon),
                                  Model_.commonPropertiesTargetObjectsOf = (Model_.commonPropertiesTargetObjectsOf baseCommon),
                                  Model_.commonPropertiesTargetSubjectsOf = (Model_.commonPropertiesTargetSubjectsOf baseCommon)}
                        updatedShape = Model_.ShapeNode (Model_.NodeShape {
                              Model_.nodeShapeCommon = updatedCommon})
                    in Model_.Definition {
                      Model_.definitionIri = (Model_.definitionIri baseDef),
                      Model_.definitionTarget = updatedShape}) vertexTypes
      in (Model_.ShapesGraph (Sets.fromList defs))

-- | Convert a property type to a SHACL property shape
propertyTypeToPropertyShape :: (t0 -> Syntax.Iri) -> (Model.PropertyKey -> Syntax.Iri) -> Model.PropertyType t0 -> Model_.PropertyShape
propertyTypeToPropertyShape encodeType encodeKey pt =

      let key = Model.propertyTypeKey pt
          path = encodeKey key
          required_ = Model.propertyTypeRequired pt
          dtIri = encodeType (Model.propertyTypeValue pt)
          constraints = Sets.singleton (Model_.CommonConstraintDatatype dtIri)
          propConstraints = Logic.ifElse required_ (Sets.singleton (Model_.PropertyShapeConstraintMinCount 1)) Sets.empty
      in Model_.PropertyShape {
        Model_.propertyShapeCommon = Model_.CommonProperties {
          Model_.commonPropertiesConstraints = constraints,
          Model_.commonPropertiesDeactivated = Nothing,
          Model_.commonPropertiesMessage = Utils.emptyLangStrings,
          Model_.commonPropertiesSeverity = Model_.SeverityViolation,
          Model_.commonPropertiesTargetClass = Sets.empty,
          Model_.commonPropertiesTargetNode = Sets.empty,
          Model_.commonPropertiesTargetObjectsOf = Sets.empty,
          Model_.commonPropertiesTargetSubjectsOf = Sets.empty},
        Model_.propertyShapeConstraints = propConstraints,
        Model_.propertyShapeDefaultValue = Nothing,
        Model_.propertyShapeDescription = Utils.emptyLangStrings,
        Model_.propertyShapeName = Utils.emptyLangStrings,
        Model_.propertyShapeOrder = Nothing,
        Model_.propertyShapePath = path}

-- | Convert a vertex type to a SHACL node shape definition
vertexTypeToNodeShape :: (t0 -> Syntax.Iri) -> (Model.VertexLabel -> Syntax.Iri) -> (Model.PropertyKey -> Syntax.Iri) -> Model.VertexType t0 -> Model_.Definition Model_.Shape
vertexTypeToNodeShape encodeType encodeLabel encodeKey vt =

      let label = Model.vertexTypeLabel vt
          labelIri = encodeLabel label
          propTypes = Model.vertexTypeProperties vt
          propShapes =
                  Lists.map (\pt -> Model_.CommonConstraintProperty (Sets.singleton (Model_.ReferenceAnonymous (propertyTypeToPropertyShape encodeType encodeKey pt)))) propTypes
          common =
                  Model_.CommonProperties {
                    Model_.commonPropertiesConstraints = (Sets.fromList propShapes),
                    Model_.commonPropertiesDeactivated = Nothing,
                    Model_.commonPropertiesMessage = Utils.emptyLangStrings,
                    Model_.commonPropertiesSeverity = Model_.SeverityViolation,
                    Model_.commonPropertiesTargetClass = (Sets.singleton (Syntax.RdfsClass ())),
                    Model_.commonPropertiesTargetNode = Sets.empty,
                    Model_.commonPropertiesTargetObjectsOf = Sets.empty,
                    Model_.commonPropertiesTargetSubjectsOf = Sets.empty}
      in Model_.Definition {
        Model_.definitionIri = labelIri,
        Model_.definitionTarget = (Model_.ShapeNode (Model_.NodeShape {
          Model_.nodeShapeCommon = common}))}
