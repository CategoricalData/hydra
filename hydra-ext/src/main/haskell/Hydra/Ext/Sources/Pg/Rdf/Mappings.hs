-- | Term-level definitions for property graph to RDF/SHACL mapping.
-- Includes both instance-level (PG data → RDF triples) and schema-level
-- (GraphSchema → SHACL ShapesGraph) mappings.

module Hydra.Ext.Sources.Pg.Rdf.Mappings where

-- Standard imports for term-level sources outside of the kernel
import Hydra.Kernel
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
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Ext.Sources.Pg.Model                as PgModel
import qualified Hydra.Ext.Sources.Pg.Rdf.Environment      as PgRdfEnvironment
import qualified Hydra.Ext.Sources.Rdf.Syntax              as RdfSyntax
import qualified Hydra.Ext.Sources.Rdf.Utils               as RdfUtils
import qualified Hydra.Ext.Sources.Shacl.Model             as ShaclModel
import qualified Hydra.Pg.Model                            as PG       -- Generated PG types
import qualified Hydra.Ext.Org.W3.Rdf.Syntax               as Rdf      -- Generated RDF types
import qualified Hydra.Ext.Org.W3.Shacl.Model              as Shacl    -- Generated SHACL types
import qualified Hydra.Dsl.Pg.Model                        as PgDsl    -- Generated PG DSL
import qualified Hydra.Dsl.Ext.Org.W3.Shacl.Model         as ShaclDsl -- Generated SHACL DSL
import qualified Hydra.Dsl.Ext.Org.W3.Rdf.Syntax          as RdfDsl   -- Generated RDF DSL


ns :: Namespace
ns = Namespace "hydra.pg.rdf.mappings"

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

module_ :: Module
module_ = Module ns elements
    [RdfUtils.ns]  -- term dependencies
    (kernelTypesNamespaces L.++ [PgModel.ns, PgRdfEnvironment.ns, RdfSyntax.ns, ShaclModel.ns]) $  -- type dependencies
    Just "Mappings from property graph schemas to SHACL shapes graphs, and from property graph data to RDF graphs"
  where
    elements = [
      toTermDefinition encodeVertex,
      toTermDefinition encodeEdge,
      toTermDefinition encodeLazyGraph,
      toTermDefinition propertyTypeToPropertyShape,
      toTermDefinition vertexTypeToNodeShape,
      toTermDefinition edgeTypesToPropertyShapes,
      toTermDefinition graphSchemaToShapesGraph]


-- PgRdfEnvironment field accessors (not yet code-generated)
_PgRdfEnvironment :: Name
_PgRdfEnvironment = Name "hydra.pg.rdf.environment.PgRdfEnvironment"

envEncodeVertexId :: TTerm a -> TTerm b
envEncodeVertexId env = project _PgRdfEnvironment (Name "encodeVertexId") @@ env

envEncodeVertexLabel :: TTerm a -> TTerm b
envEncodeVertexLabel env = project _PgRdfEnvironment (Name "encodeVertexLabel") @@ env

envEncodeEdgeLabel :: TTerm a -> TTerm b
envEncodeEdgeLabel env = project _PgRdfEnvironment (Name "encodeEdgeLabel") @@ env

envEncodePropertyKey :: TTerm a -> TTerm b
envEncodePropertyKey env = project _PgRdfEnvironment (Name "encodePropertyKey") @@ env

envEncodePropertyValue :: TTerm a -> TTerm b
envEncodePropertyValue env = project _PgRdfEnvironment (Name "encodePropertyValue") @@ env


--------------------------------------------------------------------------------
-- Instance-level mappings: property graph data to RDF

-- | Encode a property graph vertex as an RDF description.
-- The vertex id becomes the subject IRI, the vertex label becomes an rdf:type triple,
-- and each property becomes a triple with the property key as predicate and value as object.
encodeVertex :: TBinding (env -> PG.Vertex v -> Rdf.Description)
encodeVertex = define "encodeVertex" $
  doc "Encode a property graph vertex as an RDF description" $
  lambda "env" $ lambda "vertex" $
    lets [
      "vlab">: PgDsl.vertexLabel (var "vertex"),
      "vid">: PgDsl.vertexId (var "vertex"),
      "vprops">: PgDsl.vertexProperties (var "vertex"),
      "subj">: RdfDsl.resourceIri (envEncodeVertexId (var "env") @@ var "vid"),
      "rtype">: RdfDsl.nodeIri (envEncodeVertexLabel (var "env") @@ var "vlab"),
      "typeTriple">: RdfDsl.triple (var "subj") (asTerm RdfUtils.rdfIri @@ string "type") (var "rtype"),
      "propTriples">: Lists.map
        (lambda "kv" $
          lets [
            "key">: Pairs.first (var "kv"),
            "val">: Pairs.second (var "kv"),
            "pred">: envEncodePropertyKey (var "env") @@ var "key",
            "obj">: RdfDsl.nodeLiteral (envEncodePropertyValue (var "env") @@ var "val")
          ] $
          RdfDsl.triple (var "subj") (var "pred") (var "obj"))
        (Maps.toList (var "vprops")),
      "allTriples">: Lists.cons (var "typeTriple") (var "propTriples")
    ] $
    RdfDsl.description
      (asTerm RdfUtils.resourceToNode @@ var "subj")
      (RdfDsl.graph (Sets.fromList (var "allTriples")))


-- | Encode a property graph edge as an RDF description.
-- The out-vertex becomes the subject, the edge label becomes the predicate,
-- and the in-vertex becomes the object. Edge id and properties are discarded.
encodeEdge :: TBinding (env -> PG.Edge v -> Rdf.Description)
encodeEdge = define "encodeEdge" $
  doc "Encode a property graph edge as an RDF description" $
  lambda "env" $ lambda "edge" $
    lets [
      "elab">: PgDsl.edgeLabel (var "edge"),
      "eout">: PgDsl.edgeOut (var "edge"),
      "ein">: PgDsl.edgeIn (var "edge"),
      "subj">: RdfDsl.resourceIri (envEncodeVertexId (var "env") @@ var "eout"),
      "obj">: RdfDsl.nodeIri (envEncodeVertexId (var "env") @@ var "ein"),
      "pred">: envEncodeEdgeLabel (var "env") @@ var "elab"
    ] $
    RdfDsl.description
      (asTerm RdfUtils.resourceToNode @@ var "subj")
      (RdfDsl.graph (Sets.singleton (RdfDsl.triple (var "subj") (var "pred") (var "obj"))))


-- | Encode an entire lazy property graph as an RDF graph.
-- Encodes all vertices and edges as descriptions, then merges them into a single graph.
encodeLazyGraph :: TBinding (env -> PG.LazyGraph v -> Rdf.Graph)
encodeLazyGraph = define "encodeLazyGraph" $
  doc "Encode a lazy property graph as an RDF graph" $
  lambda "env" $ lambda "lg" $
    lets [
      "vertexDescs">: Lists.map (encodeVertex @@ var "env") (PgDsl.lazyGraphVertices (var "lg")),
      "edgeDescs">: Lists.map (encodeEdge @@ var "env") (PgDsl.lazyGraphEdges (var "lg")),
      "allDescs">: Lists.concat (list [var "vertexDescs", var "edgeDescs"])
    ] $
    asTerm RdfUtils.descriptionsToGraph @@ var "allDescs"


--------------------------------------------------------------------------------
-- Schema-level mappings: property graph schema to SHACL shapes

-- | A helper for creating empty SHACL common properties with only constraints specified.
emptyCommonWith :: TTerm (S.Set Shacl.CommonConstraint) -> TTerm (S.Set Rdf.RdfsClass) -> TTerm Shacl.CommonProperties
emptyCommonWith constraints targetClasses = ShaclDsl.commonProperties
  constraints                               -- constraints
  nothing                                   -- deactivated
  (asTerm RdfUtils.emptyLangStrings)        -- message
  ShaclDsl.severityViolation                -- severity
  targetClasses                             -- targetClass
  Sets.empty                                -- targetNode
  Sets.empty                                -- targetObjectsOf
  Sets.empty                                -- targetSubjectsOf

-- | A helper for creating a SHACL property shape with minimal fields.
simplePropertyShape :: TTerm Shacl.CommonProperties -> TTerm (S.Set Shacl.PropertyShapeConstraint)
  -> TTerm Rdf.Iri -> TTerm Shacl.PropertyShape
simplePropertyShape common constraints path = ShaclDsl.propertyShape
  common                                    -- common
  constraints                               -- constraints
  nothing                                   -- defaultValue
  (asTerm RdfUtils.emptyLangStrings)        -- description
  (asTerm RdfUtils.emptyLangStrings)        -- name
  nothing                                   -- order
  path                                      -- path


-- | Convert a PropertyType to a SHACL PropertyShape.
-- The property key becomes the sh:path, and the type becomes a sh:datatype constraint.
-- If the property is required, sh:minCount is set to 1.
propertyTypeToPropertyShape
  :: TBinding ((t -> Rdf.Iri) -> (PG.PropertyKey -> Rdf.Iri) -> PG.PropertyType t -> Shacl.PropertyShape)
propertyTypeToPropertyShape = define "propertyTypeToPropertyShape" $
  doc "Convert a property type to a SHACL property shape" $
  lambda "encodeType" $ lambda "encodeKey" $ lambda "pt" $
    lets [
      "key">: PgDsl.propertyTypeKey (var "pt"),
      "path">: var "encodeKey" @@ var "key",
      "required_">: PgDsl.propertyTypeRequired (var "pt"),
      "dtIri">: var "encodeType" @@ PgDsl.propertyTypeValue (var "pt"),
      "constraints">: Sets.singleton (ShaclDsl.commonConstraintDatatype (var "dtIri")),
      "propConstraints">:
        Logic.ifElse (var "required_")
          (Sets.singleton (ShaclDsl.propertyShapeConstraintMinCount (bigint 1)))
          Sets.empty
    ] $
    simplePropertyShape
      (emptyCommonWith (var "constraints") Sets.empty)
      (var "propConstraints")
      (var "path")


-- | Convert a VertexType to a SHACL Definition<Shape>, producing a NodeShape
-- with sh:targetClass set to the vertex label IRI, and property shapes for
-- each property type in the vertex type.
vertexTypeToNodeShape
  :: TBinding ((t -> Rdf.Iri) -> (PG.VertexLabel -> Rdf.Iri) -> (PG.PropertyKey -> Rdf.Iri)
    -> PG.VertexType t -> Shacl.Definition Shacl.Shape)
vertexTypeToNodeShape = define "vertexTypeToNodeShape" $
  doc "Convert a vertex type to a SHACL node shape definition" $
  lambda "encodeType" $ lambda "encodeLabel" $ lambda "encodeKey" $ lambda "vt" $
    lets [
      "label">: PgDsl.vertexTypeLabel (var "vt"),
      "labelIri">: var "encodeLabel" @@ var "label",
      "propTypes">: PgDsl.vertexTypeProperties (var "vt"),
      "propShapes">: Lists.map
        (lambda "pt" $
          ShaclDsl.commonConstraintProperty (Sets.singleton
            (ShaclDsl.referenceAnonymous
              (propertyTypeToPropertyShape
                @@ var "encodeType" @@ var "encodeKey" @@ var "pt"))))
        (var "propTypes"),
      "common">: emptyCommonWith
        (Sets.fromList (var "propShapes"))
        (Sets.singleton (RdfDsl.rdfsClass unit))
    ] $
    ShaclDsl.definition
      (var "labelIri")
      (ShaclDsl.shapeNode (ShaclDsl.nodeShape (var "common")))


-- | Convert a list of edge types into property shape constraints for a given vertex type.
-- For each edge type whose out-vertex label matches the vertex label, produces a
-- CommonConstraint.property entry with sh:path set to the edge label IRI and
-- sh:class set to the in-vertex's class IRI.
edgeTypesToPropertyShapes
  :: TBinding ((PG.VertexLabel -> Rdf.Iri) -> (PG.EdgeLabel -> Rdf.Iri) -> PG.VertexLabel
    -> [PG.EdgeType t] -> [Shacl.CommonConstraint])
edgeTypesToPropertyShapes = define "edgeTypesToPropertyShapes" $
  doc "Convert edge types into property shape constraints for a given vertex label" $
  lambda "encodeVertexLabel" $ lambda "encodeEdgeLabel" $ lambda "vertexLabel" $ lambda "edgeTypes" $
    Lists.concat (Lists.map
      (lambda "et" $
        lets [
          "outLabel">: PgDsl.edgeTypeOut (var "et"),
          "matchesVertex">: Equality.equal
            (unwrap PG._VertexLabel @@ var "outLabel")
            (unwrap PG._VertexLabel @@ var "vertexLabel")
        ] $
        lets [
          "edgeShape">: ShaclDsl.commonConstraintProperty (Sets.singleton
            (ShaclDsl.referenceAnonymous (simplePropertyShape
              (emptyCommonWith
                (Sets.singleton (ShaclDsl.commonConstraintClass
                  (Sets.singleton (RdfDsl.rdfsClass unit))))
                Sets.empty)
              Sets.empty
              (var "encodeEdgeLabel" @@ PgDsl.edgeTypeLabel (var "et")))))
        ] $
        Logic.ifElse (var "matchesVertex")
          (list [var "edgeShape"])
          (TTerm (TermList []) :: TTerm [Shacl.CommonConstraint]))
      (var "edgeTypes"))


-- | Convert a GraphSchema to a SHACL ShapesGraph.
-- Each VertexType becomes a NodeShape definition with property shapes from
-- both the vertex's property types and outgoing edge types.
graphSchemaToShapesGraph
  :: TBinding ((t -> Rdf.Iri) -> (PG.VertexLabel -> Rdf.Iri) -> (PG.EdgeLabel -> Rdf.Iri)
    -> (PG.PropertyKey -> Rdf.Iri) -> PG.GraphSchema t -> Shacl.ShapesGraph)
graphSchemaToShapesGraph = define "graphSchemaToShapesGraph" $
  doc "Convert a property graph schema to a SHACL shapes graph" $
  lambda "encodeType" $ lambda "encodeVertexLabel" $ lambda "encodeEdgeLabel"
    $ lambda "encodeKey" $ lambda "schema" $
    lets [
      "vertexTypes">: Maps.elems (PgDsl.graphSchemaVertices (var "schema")),
      "edgeTypes">: Maps.elems (PgDsl.graphSchemaEdges (var "schema")),
      "defs">: Lists.map
        (lambda "vt" $
          lets [
            "baseDef">: vertexTypeToNodeShape
              @@ var "encodeType" @@ var "encodeVertexLabel" @@ var "encodeKey" @@ var "vt",
            "edgeShapes">: edgeTypesToPropertyShapes
              @@ var "encodeVertexLabel" @@ var "encodeEdgeLabel"
              @@ PgDsl.vertexTypeLabel (var "vt") @@ var "edgeTypes",
            -- Extract the node shape's common properties, merge in edge constraints
            "baseShape">: ShaclDsl.definitionTarget (var "baseDef"),
            "baseNode">:
              cases Shacl._Shape (var "baseShape") Nothing [
                Shacl._Shape_node>>: lambda "ns" $ var "ns",
                Shacl._Shape_property>>: lambda "_" $
                  ShaclDsl.nodeShape (emptyCommonWith Sets.empty Sets.empty)],
            "baseCommon">: ShaclDsl.nodeShapeCommon (var "baseNode"),
            "mergedConstraints">: Sets.union
              (ShaclDsl.commonPropertiesConstraints (var "baseCommon"))
              (Sets.fromList (var "edgeShapes")),
            "updatedCommon">: ShaclDsl.commonPropertiesWithConstraints
              (var "baseCommon") (var "mergedConstraints"),
            "updatedShape">: ShaclDsl.shapeNode (ShaclDsl.nodeShape (var "updatedCommon"))
          ] $
          ShaclDsl.definitionWithTarget (var "baseDef") (var "updatedShape"))
        (var "vertexTypes")
    ] $
    ShaclDsl.shapesGraph (Sets.fromList (var "defs"))
