-- Note: this is an automatically generated file. Do not edit.

-- | Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs

module Hydra.Pg.Rdf.Mappings where

import qualified Hydra.Ext.Org.W3.Rdf.Syntax as Syntax
import qualified Hydra.Ext.Org.W3.Shacl.Model as Model
import qualified Hydra.Ext.Rdf.Utils as Utils
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Pg.Model as Model_
import qualified Hydra.Pg.Rdf.Environment as Environment
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Convert edge types into property shape constraints for a given vertex label
edgeTypesToPropertyShapes :: t0 -> (Model_.EdgeLabel -> Syntax.Iri) -> Model_.VertexLabel -> [Model_.EdgeType t1] -> [Model.CommonConstraint]
edgeTypesToPropertyShapes encodeVertexLabel encodeEdgeLabel vertexLabel edgeTypes =
    Lists.concat (Lists.map (\et ->
      let outLabel = Model_.edgeTypeOut et
          matchesVertex = Equality.equal (Model_.unVertexLabel outLabel) (Model_.unVertexLabel vertexLabel)
          edgeShape =
                  Model.CommonConstraintProperty (Sets.singleton (Model.ReferenceAnonymous (Model.PropertyShape {
                    Model.propertyShapeCommon = Model.CommonProperties {
                      Model.commonPropertiesConstraints = (Sets.singleton (Model.CommonConstraintClass (Sets.singleton (Syntax.RdfsClass ())))),
                      Model.commonPropertiesDeactivated = Nothing,
                      Model.commonPropertiesMessage = Utils.emptyLangStrings,
                      Model.commonPropertiesSeverity = Model.SeverityViolation,
                      Model.commonPropertiesTargetClass = Sets.empty,
                      Model.commonPropertiesTargetNode = Sets.empty,
                      Model.commonPropertiesTargetObjectsOf = Sets.empty,
                      Model.commonPropertiesTargetSubjectsOf = Sets.empty},
                    Model.propertyShapeConstraints = Sets.empty,
                    Model.propertyShapeDefaultValue = Nothing,
                    Model.propertyShapeDescription = Utils.emptyLangStrings,
                    Model.propertyShapeName = Utils.emptyLangStrings,
                    Model.propertyShapeOrder = Nothing,
                    Model.propertyShapePath = (encodeEdgeLabel (Model_.edgeTypeLabel et))})))
      in (Logic.ifElse matchesVertex [
        edgeShape] [])) edgeTypes)

-- | Encode a property graph edge as an RDF description
encodeEdge :: Environment.PgRdfEnvironment t0 -> Model_.Edge t0 -> Syntax.Description
encodeEdge env edge =

      let elab = Model_.edgeLabel edge
          eout = Model_.edgeOut edge
          ein = Model_.edgeIn edge
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
encodeLazyGraph :: Environment.PgRdfEnvironment t0 -> Model_.LazyGraph t0 -> Syntax.Graph
encodeLazyGraph env lg =

      let vertexDescs = Lists.map (encodeVertex env) (Model_.lazyGraphVertices lg)
          edgeDescs = Lists.map (encodeEdge env) (Model_.lazyGraphEdges lg)
          allDescs =
                  Lists.concat [
                    vertexDescs,
                    edgeDescs]
      in (Utils.descriptionsToGraph allDescs)

-- | Encode a property graph vertex as an RDF description
encodeVertex :: Environment.PgRdfEnvironment t0 -> Model_.Vertex t0 -> Syntax.Description
encodeVertex env vertex =

      let vlab = Model_.vertexLabel vertex
          vid = Model_.vertexId vertex
          vprops = Model_.vertexProperties vertex
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
graphSchemaToShapesGraph :: (t0 -> Syntax.Iri) -> (Model_.VertexLabel -> Syntax.Iri) -> (Model_.EdgeLabel -> Syntax.Iri) -> (Model_.PropertyKey -> Syntax.Iri) -> Model_.GraphSchema t0 -> Model.ShapesGraph
graphSchemaToShapesGraph encodeType encodeVertexLabel encodeEdgeLabel encodeKey schema =

      let vertexTypes = Maps.elems (Model_.graphSchemaVertices schema)
          edgeTypes = Maps.elems (Model_.graphSchemaEdges schema)
          defs =
                  Lists.map (\vt ->
                    let baseDef = vertexTypeToNodeShape encodeType encodeVertexLabel encodeKey vt
                        edgeShapes = edgeTypesToPropertyShapes encodeVertexLabel encodeEdgeLabel (Model_.vertexTypeLabel vt) edgeTypes
                        baseShape = Model.definitionTarget baseDef
                        baseNode =
                                case baseShape of
                                  Model.ShapeNode v0 -> v0
                                  Model.ShapeProperty _ -> Model.NodeShape {
                                    Model.nodeShapeCommon = Model.CommonProperties {
                                      Model.commonPropertiesConstraints = Sets.empty,
                                      Model.commonPropertiesDeactivated = Nothing,
                                      Model.commonPropertiesMessage = Utils.emptyLangStrings,
                                      Model.commonPropertiesSeverity = Model.SeverityViolation,
                                      Model.commonPropertiesTargetClass = Sets.empty,
                                      Model.commonPropertiesTargetNode = Sets.empty,
                                      Model.commonPropertiesTargetObjectsOf = Sets.empty,
                                      Model.commonPropertiesTargetSubjectsOf = Sets.empty}}
                        baseCommon = Model.nodeShapeCommon baseNode
                        mergedConstraints = Sets.union (Model.commonPropertiesConstraints baseCommon) (Sets.fromList edgeShapes)
                        updatedCommon =
                                Model.CommonProperties {
                                  Model.commonPropertiesConstraints = mergedConstraints,
                                  Model.commonPropertiesDeactivated = (Model.commonPropertiesDeactivated baseCommon),
                                  Model.commonPropertiesMessage = (Model.commonPropertiesMessage baseCommon),
                                  Model.commonPropertiesSeverity = (Model.commonPropertiesSeverity baseCommon),
                                  Model.commonPropertiesTargetClass = (Model.commonPropertiesTargetClass baseCommon),
                                  Model.commonPropertiesTargetNode = (Model.commonPropertiesTargetNode baseCommon),
                                  Model.commonPropertiesTargetObjectsOf = (Model.commonPropertiesTargetObjectsOf baseCommon),
                                  Model.commonPropertiesTargetSubjectsOf = (Model.commonPropertiesTargetSubjectsOf baseCommon)}
                        updatedShape = Model.ShapeNode (Model.NodeShape {
                              Model.nodeShapeCommon = updatedCommon})
                    in Model.Definition {
                      Model.definitionIri = (Model.definitionIri baseDef),
                      Model.definitionTarget = updatedShape}) vertexTypes
      in (Model.ShapesGraph (Sets.fromList defs))

-- | Convert a property type to a SHACL property shape
propertyTypeToPropertyShape :: (t0 -> Syntax.Iri) -> (Model_.PropertyKey -> Syntax.Iri) -> Model_.PropertyType t0 -> Model.PropertyShape
propertyTypeToPropertyShape encodeType encodeKey pt =

      let key = Model_.propertyTypeKey pt
          path = encodeKey key
          required_ = Model_.propertyTypeRequired pt
          dtIri = encodeType (Model_.propertyTypeValue pt)
          constraints = Sets.singleton (Model.CommonConstraintDatatype dtIri)
          propConstraints = Logic.ifElse required_ (Sets.singleton (Model.PropertyShapeConstraintMinCount 1)) Sets.empty
      in Model.PropertyShape {
        Model.propertyShapeCommon = Model.CommonProperties {
          Model.commonPropertiesConstraints = constraints,
          Model.commonPropertiesDeactivated = Nothing,
          Model.commonPropertiesMessage = Utils.emptyLangStrings,
          Model.commonPropertiesSeverity = Model.SeverityViolation,
          Model.commonPropertiesTargetClass = Sets.empty,
          Model.commonPropertiesTargetNode = Sets.empty,
          Model.commonPropertiesTargetObjectsOf = Sets.empty,
          Model.commonPropertiesTargetSubjectsOf = Sets.empty},
        Model.propertyShapeConstraints = propConstraints,
        Model.propertyShapeDefaultValue = Nothing,
        Model.propertyShapeDescription = Utils.emptyLangStrings,
        Model.propertyShapeName = Utils.emptyLangStrings,
        Model.propertyShapeOrder = Nothing,
        Model.propertyShapePath = path}

-- | Convert a vertex type to a SHACL node shape definition
vertexTypeToNodeShape :: (t0 -> Syntax.Iri) -> (Model_.VertexLabel -> Syntax.Iri) -> (Model_.PropertyKey -> Syntax.Iri) -> Model_.VertexType t0 -> Model.Definition Model.Shape
vertexTypeToNodeShape encodeType encodeLabel encodeKey vt =

      let label = Model_.vertexTypeLabel vt
          labelIri = encodeLabel label
          propTypes = Model_.vertexTypeProperties vt
          propShapes =
                  Lists.map (\pt -> Model.CommonConstraintProperty (Sets.singleton (Model.ReferenceAnonymous (propertyTypeToPropertyShape encodeType encodeKey pt)))) propTypes
          common =
                  Model.CommonProperties {
                    Model.commonPropertiesConstraints = (Sets.fromList propShapes),
                    Model.commonPropertiesDeactivated = Nothing,
                    Model.commonPropertiesMessage = Utils.emptyLangStrings,
                    Model.commonPropertiesSeverity = Model.SeverityViolation,
                    Model.commonPropertiesTargetClass = (Sets.singleton (Syntax.RdfsClass ())),
                    Model.commonPropertiesTargetNode = Sets.empty,
                    Model.commonPropertiesTargetObjectsOf = Sets.empty,
                    Model.commonPropertiesTargetSubjectsOf = Sets.empty}
      in Model.Definition {
        Model.definitionIri = labelIri,
        Model.definitionTarget = (Model.ShapeNode (Model.NodeShape {
          Model.nodeShapeCommon = common}))}
