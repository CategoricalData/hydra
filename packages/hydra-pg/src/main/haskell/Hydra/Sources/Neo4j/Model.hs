module Hydra.Sources.Neo4j.Model where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:), (@@))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.neo4j.model"

define :: String -> Type -> TypeDefinition
define = defineType ns

neo4j :: String -> Type
neo4j = typeref ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A data model for Neo4j-flavored property graphs, "
              ++ "aligned with the types of the Neo4j Java driver (org.neo4j.driver) at the instance level "
              ++ "and with Neo4j's constraints and graph types at the schema level."))}
  where
    definitions = [
      constraint,
      constraintDefinition,
      element,
      elementId,
      graphType,
      isoDuration,
      key,
      keyConstraint,
      localDate,
      localDateTime,
      localTime,
      node,
      nodeElementType,
      nodeLabel,
      offsetTime,
      path,
      point,
      propertyExistenceConstraint,
      propertyTypeConstraint,
      propertyUniquenessConstraint,
      relationship,
      relationshipElementType,
      relationshipType,
      segment,
      spatialReferenceIdentifier,
      value,
      valueType,
      vectorType,
      zoneId,
      zoneOffset,
      zonedDateTime]

constraint :: TypeDefinition
constraint = define "Constraint" $
  doc ("A Neo4j schema constraint, as one of the four constraint types Neo4j supports. The node-vs-"
    ++ "relationship flavor is determined by the element type the constraint is attached to, not by "
    ++ "the constraint itself.") $
  T.union [
    "key">:
      doc "A key constraint" $
      neo4j "KeyConstraint",
    "propertyExistence">:
      doc "A property existence constraint" $
      neo4j "PropertyExistenceConstraint",
    "propertyType">:
      doc "A property type constraint" $
      neo4j "PropertyTypeConstraint",
    "propertyUniqueness">:
      doc "A property uniqueness constraint" $
      neo4j "PropertyUniquenessConstraint"]

constraintDefinition :: TypeDefinition
constraintDefinition = define "ConstraintDefinition" $
  doc ("A named constraint. The name is optional at authoring time; Neo4j auto-generates one if it is "
    ++ "not supplied. The name is operational metadata (used to reference the constraint for dropping, "
    ++ "idempotent creation, and reporting) and has no effect on what the constraint enforces.") $
  T.record [
    "name">:
      doc "An optional name for the constraint" $
      T.optional T.string,
    "body">:
      doc "The constraint itself" $
      neo4j "Constraint"]

element :: TypeDefinition
element = define "Element" $
  doc ("Either a node or a relationship; the common supertype of the two kinds of graph element. "
    ++ "A bare union with no fields of its own: the id and properties live on each variant. "
    ++ "Named Element (not Entity, the driver's interface name) to avoid the Neo4j docs' use of "
    ++ "\"entity\" for a node's real-world referent, and to match hydra.pg.model and the root of elementId.") $
  T.union [
    "node">:
      doc "The element is a node" $
      neo4j "Node",
    "relationship">:
      doc "The element is a relationship" $
      neo4j "Relationship"]

elementId :: TypeDefinition
elementId = define "ElementId" $
  doc "The element id of a node or relationship (the driver's String-valued elementId())" $
  T.wrap T.string

graphType :: TypeDefinition
graphType = define "GraphType" $
  doc ("A Neo4j graph type: a holistic schema for a graph, made up of node element types and "
    ++ "relationship element types (Neo4j's GRAPH TYPE feature).") $
  T.record [
    "nodes">:
      doc "The node element types of the graph type" $
      T.list (neo4j "NodeElementType"),
    "relationships">:
      doc "The relationship element types of the graph type" $
      T.list (neo4j "RelationshipElementType")]

isoDuration :: TypeDefinition
isoDuration = define "IsoDuration" $
  doc ("A temporal amount, following the Neo4j driver's IsoDuration. The four components are kept "
    ++ "separate and non-normalized, since a month is not a fixed number of days.") $
  T.record [
    "months">:
      doc "A number of months" $
      T.int64,
    "days">:
      doc "A number of days" $
      T.int64,
    "seconds">:
      doc "A number of seconds" $
      T.int64,
    "nanoseconds">:
      doc "A number of nanoseconds of the second" $
      T.int32]

key :: TypeDefinition
key = define "Key" $
  doc ("A key in a property map or value map. Used for all maps, following the driver's single key "
    ++ "notion (MapAccessor.keys()/get(String key)); there is no distinct \"property key\" type.") $
  T.wrap T.string

keyConstraint :: TypeDefinition
keyConstraint = define "KeyConstraint" $
  doc ("A key constraint: the listed properties must all exist and their combined values must be "
    ++ "unique. Neo4j's primary-key analog, combining existence and uniqueness over a set of "
    ++ "properties. Applies to a node label or a relationship type depending on where it is attached.") $
  T.record [
    "properties">:
      doc "The properties whose combination must exist and be unique" $
      T.list (neo4j "Key")]

localDate :: TypeDefinition
localDate = define "LocalDate" $
  doc "A date without a time or time zone, following java.time.LocalDate (the driver's asLocalDate())" $
  T.record [
    "year">:
      doc "The year" $
      T.int32,
    "month">:
      doc "The month of the year" $
      T.int32,
    "day">:
      doc "The day of the month" $
      T.int32]

localDateTime :: TypeDefinition
localDateTime = define "LocalDateTime" $
  doc "A date and time without a time zone, following java.time.LocalDateTime (the driver's asLocalDateTime())" $
  T.record [
    "date">:
      doc "The date part" $
      neo4j "LocalDate",
    "time">:
      doc "The time part" $
      neo4j "LocalTime"]

localTime :: TypeDefinition
localTime = define "LocalTime" $
  doc "A time of day without a time zone, following java.time.LocalTime (the driver's asLocalTime())" $
  T.record [
    "hour">:
      doc "The hour of the day" $
      T.int32,
    "minute">:
      doc "The minute of the hour" $
      T.int32,
    "second">:
      doc "The second of the minute" $
      T.int32,
    "nanosecond">:
      doc "The nanosecond of the second" $
      T.int32]

node :: TypeDefinition
node = define "Node" $
  doc "A node; a graph element with a set of labels and a map of properties" $
  T.record [
    "id">:
      doc "The element id of the node" $
      neo4j "ElementId",
    "labels">:
      doc "The set of labels of the node (a node may have zero or more labels)" $
      T.set (neo4j "NodeLabel"),
    "properties">:
      doc "The properties of the node" $
      T.map (neo4j "Key") (neo4j "Value")]

nodeElementType :: TypeDefinition
nodeElementType = define "NodeElementType" $
  doc ("A node element type within a graph type: an identifying label, any implied labels it entails, "
    ++ "and the constraints on its properties. A node's typed properties are expressed as property "
    ++ "type constraints (and existence constraints for non-null ones), since Neo4j has no untyped "
    ++ "property declaration.") $
  T.record [
    "identifyingLabel">:
      doc "The identifying label of the node element type" $
      neo4j "NodeLabel",
    "impliedLabels">:
      doc "Labels that a node of this type must also have (label implication)" $
      T.set (neo4j "NodeLabel"),
    "constraints">:
      doc "The constraints on nodes of this type" $
      T.list (neo4j "ConstraintDefinition")]

nodeLabel :: TypeDefinition
nodeLabel = define "NodeLabel" $
  doc ("A label applied to a node. Nodes carry a set of labels; relationships have a single type "
    ++ "(see RelationshipType), so there is no relationship-label concept.") $
  T.wrap T.string

offsetTime :: TypeDefinition
offsetTime = define "OffsetTime" $
  doc ("A time of day with a UTC offset, following java.time.OffsetTime (the driver's asOffsetTime()). "
    ++ "Distinct from LocalTime by virtue of the offset.") $
  T.record [
    "time">:
      doc "The local time" $
      neo4j "LocalTime",
    "offset">:
      doc "The offset from UTC" $
      neo4j "ZoneOffset"]

path :: TypeDefinition
path = define "Path" $
  doc ("A path through the graph: a start node followed by a sequence of segments. The node sequence is "
    ++ "the start node followed by each segment's end node; a zero-length path has an empty segment list. "
    ++ "Each segment carries full node and relationship objects, so a path is self-contained.") $
  T.record [
    "start">:
      doc "The first node of the path" $
      neo4j "Node",
    "segments">:
      doc "The segments of the path, in order (empty for a zero-length path)" $
      T.list (neo4j "Segment")]

point :: TypeDefinition
point = define "Point" $
  doc ("A spatial point, following the Neo4j driver's Point. The coordinate reference system is given by "
    ++ "the srid, which also determines the units and dimensionality: under WGS-84 (srid 4326/4979), x is "
    ++ "longitude and y is latitude in decimal degrees and z is height in meters; under Cartesian "
    ++ "(srid 7203/9157), the units are unspecified. A two-dimensional point omits z.") $
  T.record [
    "srid">:
      doc "The identifier of the coordinate reference system for this point" $
      neo4j "SpatialReferenceIdentifier",
    "x">:
      doc "The x coordinate (longitude under WGS-84)" $
      T.float64,
    "y">:
      doc "The y coordinate (latitude under WGS-84)" $
      T.float64,
    "z">:
      doc "The z coordinate (height under WGS-84); absent for a two-dimensional point" $
      T.optional T.float64]

propertyExistenceConstraint :: TypeDefinition
propertyExistenceConstraint = define "PropertyExistenceConstraint" $
  doc "A property existence constraint: the given property must exist (be non-null)." $
  T.record [
    "property">:
      doc "The property that must exist" $
      neo4j "Key"]

propertyTypeConstraint :: TypeDefinition
propertyTypeConstraint = define "PropertyTypeConstraint" $
  doc "A property type constraint: the given property must have the given value type." $
  T.record [
    "property">:
      doc "The property that is constrained" $
      neo4j "Key",
    "type">:
      doc "The type the property's value must have" $
      neo4j "ValueType"]

propertyUniquenessConstraint :: TypeDefinition
propertyUniquenessConstraint = define "PropertyUniquenessConstraint" $
  doc ("A property uniqueness constraint: the combined values of the listed properties must be unique. "
    ++ "Unlike a key constraint, it does not require the properties to exist.") $
  T.record [
    "properties">:
      doc "The properties whose combined values must be unique" $
      T.list (neo4j "Key")]

relationship :: TypeDefinition
relationship = define "Relationship" $
  doc ("A relationship; a directed graph element with a single type, connecting a start node to an end "
    ++ "node. Relationships are always stored directed; undirected traversal is a query-level concern.") $
  T.record [
    "id">:
      doc "The element id of the relationship" $
      neo4j "ElementId",
    "properties">:
      doc "The properties of the relationship" $
      T.map (neo4j "Key") (neo4j "Value"),
    "type">:
      doc "The type of the relationship" $
      neo4j "RelationshipType",
    "start">:
      doc "The element id of the start node" $
      neo4j "ElementId",
    "end">:
      doc "The element id of the end node" $
      neo4j "ElementId"]

relationshipElementType :: TypeDefinition
relationshipElementType = define "RelationshipElementType" $
  doc ("A relationship element type within a graph type: a relationship type, the node labels it may "
    ++ "connect, and the constraints on its properties.") $
  T.record [
    "type">:
      doc "The relationship type" $
      neo4j "RelationshipType",
    "startLabel">:
      doc "The label of the start node" $
      neo4j "NodeLabel",
    "endLabel">:
      doc "The label of the end node" $
      neo4j "NodeLabel",
    "constraints">:
      doc "The constraints on relationships of this type" $
      T.list (neo4j "ConstraintDefinition")]

relationshipType :: TypeDefinition
relationshipType = define "RelationshipType" $
  doc "The type of a relationship. A relationship has exactly one type." $
  T.wrap T.string

segment :: TypeDefinition
segment = define "Segment" $
  doc ("One step of a path: a relationship and the node it arrives at. The segment's implicit start is "
    ++ "the previous segment's end node, or the path's start node for the first segment, so it is not "
    ++ "stored here.") $
  T.record [
    "relationship">:
      doc "The relationship traversed in this segment" $
      neo4j "Relationship",
    "end">:
      doc "The node this segment arrives at" $
      neo4j "Node"]

spatialReferenceIdentifier :: TypeDefinition
spatialReferenceIdentifier = define "SpatialReferenceIdentifier" $
  doc ("A spatial reference identifier (SRID): an integer naming the coordinate reference system in "
    ++ "which a point's coordinates are expressed. Neo4j supports four: WGS-84 (4326), WGS-84 3D (4979), "
    ++ "Cartesian (7203), and Cartesian 3D (9157).") $
  T.wrap T.int32

value :: TypeDefinition
value = define "Value" $
  doc ("A Neo4j value: the general container for instance data, analogous to Hydra's Term. It can "
    ++ "represent anything the driver's Value can be (the full TypeSystem), including structural and "
    ++ "result-shaped values such as maps, nodes, relationships, paths, and null. Which values are valid "
    ++ "as a stored property is a schema/validation concern, not a restriction on Value itself.") $
  T.union [
    "boolean">:
      doc "A boolean value" $
      T.boolean,
    "bytes">:
      doc "A byte array" $
      T.binary,
    "date">:
      doc "A date" $
      neo4j "LocalDate",
    "dateTime">:
      doc "A date and time with a zone" $
      neo4j "ZonedDateTime",
    "duration">:
      doc "A temporal amount" $
      neo4j "IsoDuration",
    "float">:
      doc "A 64-bit floating-point number" $
      T.float64,
    "integer">:
      doc "A 64-bit integer" $
      T.int64,
    "list">:
      doc "A list of values" $
      T.list (neo4j "Value"),
    "localDateTime">:
      doc "A date and time without a zone" $
      neo4j "LocalDateTime",
    "localTime">:
      doc "A time of day without a zone" $
      neo4j "LocalTime",
    "map">:
      doc "A map from keys to values" $
      T.map (neo4j "Key") (neo4j "Value"),
    "node">:
      doc "A node" $
      neo4j "Node",
    "null">:
      doc "A null value" $
      T.unit,
    "path">:
      doc "A path" $
      neo4j "Path",
    "point">:
      doc "A spatial point" $
      neo4j "Point",
    "relationship">:
      doc "A relationship" $
      neo4j "Relationship",
    "string">:
      doc "A string value" $
      T.string,
    "time">:
      doc "A time of day with a UTC offset" $
      neo4j "OffsetTime"]

valueType :: TypeDefinition
valueType = define "ValueType" $
  doc ("A Neo4j property type, as used in property type constraints and graph type property "
    ++ "declarations (the IS :: <TYPE> expression language). Atoms mirror the scalar, temporal, and "
    ++ "spatial kinds of Value; lists, vectors, and closed unions compose them.") $
  T.union [
    "boolean">:
      doc "The BOOLEAN type" $
      T.unit,
    "string">:
      doc "The STRING type" $
      T.unit,
    "integer">:
      doc "The INTEGER type" $
      T.unit,
    "float">:
      doc "The FLOAT type" $
      T.unit,
    "date">:
      doc "The DATE type" $
      T.unit,
    "localTime">:
      doc "The LOCAL TIME type" $
      T.unit,
    "zonedTime">:
      doc "The ZONED TIME type" $
      T.unit,
    "localDateTime">:
      doc "The LOCAL DATETIME type" $
      T.unit,
    "zonedDateTime">:
      doc "The ZONED DATETIME type" $
      T.unit,
    "duration">:
      doc "The DURATION type" $
      T.unit,
    "point">:
      doc "The POINT type" $
      T.unit,
    "list">:
      doc "A LIST type, with the given element type" $
      neo4j "ValueType",
    "vector">:
      doc "A VECTOR type, with an element type and a dimension" $
      neo4j "VectorType",
    "union">:
      doc "A closed union of the given types" $
      T.list (neo4j "ValueType")]

vectorType :: TypeDefinition
vectorType = define "VectorType" $
  doc "A Neo4j VECTOR type: an element type and a fixed dimension." $
  T.record [
    "element">:
      doc "The element type of the vector" $
      neo4j "ValueType",
    "dimension">:
      doc "The dimension (length) of the vector" $
      T.int32]

zoneId :: TypeDefinition
zoneId = define "ZoneId" $
  doc ("A time-zone identifier, following java.time.ZoneId: an IANA region id such as \"Europe/Paris\" "
    ++ "(it may also be a fixed-offset string).") $
  T.wrap T.string

zoneOffset :: TypeDefinition
zoneOffset = define "ZoneOffset" $
  doc ("An offset from UTC, following java.time.ZoneOffset, expressed as a total number of seconds "
    ++ "(for example, +01:00 is 3600).") $
  T.wrap T.int32

zonedDateTime :: TypeDefinition
zonedDateTime = define "ZonedDateTime" $
  doc ("A date and time with a time zone, following java.time.ZonedDateTime (the driver's "
    ++ "asZonedDateTime()). It keeps both the resolved offset and the zone: the zone determines the "
    ++ "offset rules, while the offset is the concrete UTC delta at this instant.") $
  T.record [
    "dateTime">:
      doc "The local date and time" $
      neo4j "LocalDateTime",
    "offset">:
      doc "The resolved offset from UTC at this instant" $
      neo4j "ZoneOffset",
    "zone">:
      doc "The time zone" $
      neo4j "ZoneId"]
