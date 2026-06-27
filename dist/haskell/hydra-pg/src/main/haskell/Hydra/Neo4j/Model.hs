-- Note: this is an automatically generated file. Do not edit.
-- | A data model for Neo4j-flavored property graphs, aligned with the types of the Neo4j Java driver (org.neo4j.driver) at the instance level and with Neo4j's constraints and graph types at the schema level.

module Hydra.Neo4j.Model where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.Map as M
import qualified Data.Set as S
-- | A Neo4j schema constraint, as one of the four constraint types Neo4j supports. The node-vs-relationship flavor is determined by the element type the constraint is attached to, not by the constraint itself.
data Constraint =
  -- | A key constraint
  ConstraintKey KeyConstraint |
  -- | A property existence constraint
  ConstraintPropertyExistence PropertyExistenceConstraint |
  -- | A property type constraint
  ConstraintPropertyType PropertyTypeConstraint |
  -- | A property uniqueness constraint
  ConstraintPropertyUniqueness PropertyUniquenessConstraint
  deriving (Eq, Ord, Read, Show)
_Constraint = Core.Name "hydra.neo4j.model.Constraint"
_Constraint_key = Core.Name "key"
_Constraint_propertyExistence = Core.Name "propertyExistence"
_Constraint_propertyType = Core.Name "propertyType"
_Constraint_propertyUniqueness = Core.Name "propertyUniqueness"
-- | A named constraint. The name is optional at authoring time; Neo4j auto-generates one if it is not supplied. The name is operational metadata (used to reference the constraint for dropping, idempotent creation, and reporting) and has no effect on what the constraint enforces.
data ConstraintDefinition =
  ConstraintDefinition {
    -- | An optional name for the constraint
    constraintDefinitionName :: (Maybe String),
    -- | The constraint itself
    constraintDefinitionBody :: Constraint}
  deriving (Eq, Ord, Read, Show)
_ConstraintDefinition = Core.Name "hydra.neo4j.model.ConstraintDefinition"
_ConstraintDefinition_name = Core.Name "name"
_ConstraintDefinition_body = Core.Name "body"
-- | Either a node or a relationship; the common supertype of the two kinds of graph element. A bare union with no fields of its own: the id and properties live on each variant. Named Element (not Entity, the driver's interface name) to avoid the Neo4j docs' use of "entity" for a node's real-world referent, and to match hydra.pg.model and the root of elementId.
data Element =
  -- | The element is a node
  ElementNode Node |
  -- | The element is a relationship
  ElementRelationship Relationship
  deriving (Eq, Ord, Read, Show)
_Element = Core.Name "hydra.neo4j.model.Element"
_Element_node = Core.Name "node"
_Element_relationship = Core.Name "relationship"
-- | The element id of a node or relationship (the driver's String-valued elementId())
newtype ElementId =
  ElementId {
    unElementId :: String}
  deriving (Eq, Ord, Read, Show)
_ElementId = Core.Name "hydra.neo4j.model.ElementId"
-- | A Neo4j graph type: a holistic schema for a graph, made up of node element types and relationship element types (Neo4j's GRAPH TYPE feature).
data GraphType =
  GraphType {
    -- | The node element types of the graph type
    graphTypeNodes :: [NodeElementType],
    -- | The relationship element types of the graph type
    graphTypeRelationships :: [RelationshipElementType]}
  deriving (Eq, Ord, Read, Show)
_GraphType = Core.Name "hydra.neo4j.model.GraphType"
_GraphType_nodes = Core.Name "nodes"
_GraphType_relationships = Core.Name "relationships"
-- | A temporal amount, following the Neo4j driver's IsoDuration. The four components are kept separate and non-normalized, since a month is not a fixed number of days.
data IsoDuration =
  IsoDuration {
    -- | A number of months
    isoDurationMonths :: I.Int64,
    -- | A number of days
    isoDurationDays :: I.Int64,
    -- | A number of seconds
    isoDurationSeconds :: I.Int64,
    -- | A number of nanoseconds of the second
    isoDurationNanoseconds :: Int}
  deriving (Eq, Ord, Read, Show)
_IsoDuration = Core.Name "hydra.neo4j.model.IsoDuration"
_IsoDuration_months = Core.Name "months"
_IsoDuration_days = Core.Name "days"
_IsoDuration_seconds = Core.Name "seconds"
_IsoDuration_nanoseconds = Core.Name "nanoseconds"
-- | A key in a property map or value map. Used for all maps, following the driver's single key notion (MapAccessor.keys()/get(String key)); there is no distinct "property key" type.
newtype Key =
  Key {
    unKey :: String}
  deriving (Eq, Ord, Read, Show)
_Key = Core.Name "hydra.neo4j.model.Key"
-- | A key constraint: the listed properties must all exist and their combined values must be unique. Neo4j's primary-key analog, combining existence and uniqueness over a set of properties. Applies to a node label or a relationship type depending on where it is attached.
data KeyConstraint =
  KeyConstraint {
    -- | The properties whose combination must exist and be unique
    keyConstraintProperties :: [Key]}
  deriving (Eq, Ord, Read, Show)
_KeyConstraint = Core.Name "hydra.neo4j.model.KeyConstraint"
_KeyConstraint_properties = Core.Name "properties"
-- | A date without a time or time zone, following java.time.LocalDate (the driver's asLocalDate())
data LocalDate =
  LocalDate {
    -- | The year
    localDateYear :: Int,
    -- | The month of the year
    localDateMonth :: Int,
    -- | The day of the month
    localDateDay :: Int}
  deriving (Eq, Ord, Read, Show)
_LocalDate = Core.Name "hydra.neo4j.model.LocalDate"
_LocalDate_year = Core.Name "year"
_LocalDate_month = Core.Name "month"
_LocalDate_day = Core.Name "day"
-- | A date and time without a time zone, following java.time.LocalDateTime (the driver's asLocalDateTime())
data LocalDateTime =
  LocalDateTime {
    -- | The date part
    localDateTimeDate :: LocalDate,
    -- | The time part
    localDateTimeTime :: LocalTime}
  deriving (Eq, Ord, Read, Show)
_LocalDateTime = Core.Name "hydra.neo4j.model.LocalDateTime"
_LocalDateTime_date = Core.Name "date"
_LocalDateTime_time = Core.Name "time"
-- | A time of day without a time zone, following java.time.LocalTime (the driver's asLocalTime())
data LocalTime =
  LocalTime {
    -- | The hour of the day
    localTimeHour :: Int,
    -- | The minute of the hour
    localTimeMinute :: Int,
    -- | The second of the minute
    localTimeSecond :: Int,
    -- | The nanosecond of the second
    localTimeNanosecond :: Int}
  deriving (Eq, Ord, Read, Show)
_LocalTime = Core.Name "hydra.neo4j.model.LocalTime"
_LocalTime_hour = Core.Name "hour"
_LocalTime_minute = Core.Name "minute"
_LocalTime_second = Core.Name "second"
_LocalTime_nanosecond = Core.Name "nanosecond"
-- | The caller-supplied conversions a mapping between Hydra's property-graph model (hydra.pg.model) and this Neo4j model needs: a pair of partial functions converting property-graph values to and from Neo4j element ids, and another pair converting them to and from Neo4j property values. Errors are plain strings for now; once issue #518 generalizes the kernel Coder, these four fields collapse to two Coders. Failures (e.g. a value with no Neo4j representation) surface via the Either.
data Neo4jMapping v =
  Neo4jMapping {
    -- | Encode a property-graph value as a Neo4j element id
    neo4jMappingEncodeId :: (v -> Either String ElementId),
    -- | Decode a Neo4j element id as a property-graph value
    neo4jMappingDecodeId :: (ElementId -> Either String v),
    -- | Encode a property-graph value as a Neo4j property value
    neo4jMappingEncodeValue :: (v -> Either String Value),
    -- | Decode a Neo4j property value as a property-graph value
    neo4jMappingDecodeValue :: (Value -> Either String v)}
_Neo4jMapping = Core.Name "hydra.neo4j.model.Neo4jMapping"
_Neo4jMapping_encodeId = Core.Name "encodeId"
_Neo4jMapping_decodeId = Core.Name "decodeId"
_Neo4jMapping_encodeValue = Core.Name "encodeValue"
_Neo4jMapping_decodeValue = Core.Name "decodeValue"
-- | A node; a graph element with a set of labels and a map of properties
data Node =
  Node {
    -- | The element id of the node
    nodeId :: ElementId,
    -- | The set of labels of the node (a node may have zero or more labels)
    nodeLabels :: (S.Set NodeLabel),
    -- | The properties of the node
    nodeProperties :: (M.Map Key Value)}
  deriving (Eq, Ord, Read, Show)
_Node = Core.Name "hydra.neo4j.model.Node"
_Node_id = Core.Name "id"
_Node_labels = Core.Name "labels"
_Node_properties = Core.Name "properties"
-- | A node element type within a graph type: an identifying label, any implied labels it entails, and the constraints on its properties. A node's typed properties are expressed as property type constraints (and existence constraints for non-null ones), since Neo4j has no untyped property declaration.
data NodeElementType =
  NodeElementType {
    -- | The identifying label of the node element type
    nodeElementTypeIdentifyingLabel :: NodeLabel,
    -- | Labels that a node of this type must also have (label implication)
    nodeElementTypeImpliedLabels :: (S.Set NodeLabel),
    -- | The constraints on nodes of this type
    nodeElementTypeConstraints :: [ConstraintDefinition]}
  deriving (Eq, Ord, Read, Show)
_NodeElementType = Core.Name "hydra.neo4j.model.NodeElementType"
_NodeElementType_identifyingLabel = Core.Name "identifyingLabel"
_NodeElementType_impliedLabels = Core.Name "impliedLabels"
_NodeElementType_constraints = Core.Name "constraints"
-- | A label applied to a node. Nodes carry a set of labels; relationships have a single type (see RelationshipType), so there is no relationship-label concept.
newtype NodeLabel =
  NodeLabel {
    unNodeLabel :: String}
  deriving (Eq, Ord, Read, Show)
_NodeLabel = Core.Name "hydra.neo4j.model.NodeLabel"
-- | A time of day with a UTC offset, following java.time.OffsetTime (the driver's asOffsetTime()). Distinct from LocalTime by virtue of the offset.
data OffsetTime =
  OffsetTime {
    -- | The local time
    offsetTimeTime :: LocalTime,
    -- | The offset from UTC
    offsetTimeOffset :: ZoneOffset}
  deriving (Eq, Ord, Read, Show)
_OffsetTime = Core.Name "hydra.neo4j.model.OffsetTime"
_OffsetTime_time = Core.Name "time"
_OffsetTime_offset = Core.Name "offset"
-- | A path through the graph: a start node followed by a sequence of segments. The node sequence is the start node followed by each segment's end node; a zero-length path has an empty segment list. Each segment carries full node and relationship objects, so a path is self-contained.
data Path =
  Path {
    -- | The first node of the path
    pathStart :: Node,
    -- | The segments of the path, in order (empty for a zero-length path)
    pathSegments :: [Segment]}
  deriving (Eq, Ord, Read, Show)
_Path = Core.Name "hydra.neo4j.model.Path"
_Path_start = Core.Name "start"
_Path_segments = Core.Name "segments"
-- | A spatial point, following the Neo4j driver's Point. The coordinate reference system is given by the srid, which also determines the units and dimensionality: under WGS-84 (srid 4326/4979), x is longitude and y is latitude in decimal degrees and z is height in meters; under Cartesian (srid 7203/9157), the units are unspecified. A two-dimensional point omits z.
data Point =
  Point {
    -- | The identifier of the coordinate reference system for this point
    pointSrid :: SpatialReferenceIdentifier,
    -- | The x coordinate (longitude under WGS-84)
    pointX :: Double,
    -- | The y coordinate (latitude under WGS-84)
    pointY :: Double,
    -- | The z coordinate (height under WGS-84); absent for a two-dimensional point
    pointZ :: (Maybe Double)}
  deriving (Eq, Ord, Read, Show)
_Point = Core.Name "hydra.neo4j.model.Point"
_Point_srid = Core.Name "srid"
_Point_x = Core.Name "x"
_Point_y = Core.Name "y"
_Point_z = Core.Name "z"
-- | A property existence constraint: the given property must exist (be non-null).
data PropertyExistenceConstraint =
  PropertyExistenceConstraint {
    -- | The property that must exist
    propertyExistenceConstraintProperty :: Key}
  deriving (Eq, Ord, Read, Show)
_PropertyExistenceConstraint = Core.Name "hydra.neo4j.model.PropertyExistenceConstraint"
_PropertyExistenceConstraint_property = Core.Name "property"
-- | A property type constraint: the given property must have the given value type.
data PropertyTypeConstraint =
  PropertyTypeConstraint {
    -- | The property that is constrained
    propertyTypeConstraintProperty :: Key,
    -- | The type the property's value must have
    propertyTypeConstraintType :: ValueType}
  deriving (Eq, Ord, Read, Show)
_PropertyTypeConstraint = Core.Name "hydra.neo4j.model.PropertyTypeConstraint"
_PropertyTypeConstraint_property = Core.Name "property"
_PropertyTypeConstraint_type = Core.Name "type"
-- | A property uniqueness constraint: the combined values of the listed properties must be unique. Unlike a key constraint, it does not require the properties to exist.
data PropertyUniquenessConstraint =
  PropertyUniquenessConstraint {
    -- | The properties whose combined values must be unique
    propertyUniquenessConstraintProperties :: [Key]}
  deriving (Eq, Ord, Read, Show)
_PropertyUniquenessConstraint = Core.Name "hydra.neo4j.model.PropertyUniquenessConstraint"
_PropertyUniquenessConstraint_properties = Core.Name "properties"
-- | A relationship; a directed graph element with a single type, connecting a start node to an end node. Relationships are always stored directed; undirected traversal is a query-level concern.
data Relationship =
  Relationship {
    -- | The element id of the relationship
    relationshipId :: ElementId,
    -- | The properties of the relationship
    relationshipProperties :: (M.Map Key Value),
    -- | The type of the relationship
    relationshipType :: RelationshipType,
    -- | The element id of the start node
    relationshipStart :: ElementId,
    -- | The element id of the end node
    relationshipEnd :: ElementId}
  deriving (Eq, Ord, Read, Show)
_Relationship = Core.Name "hydra.neo4j.model.Relationship"
_Relationship_id = Core.Name "id"
_Relationship_properties = Core.Name "properties"
_Relationship_type = Core.Name "type"
_Relationship_start = Core.Name "start"
_Relationship_end = Core.Name "end"
-- | A relationship element type within a graph type: a relationship type, the node labels it may connect, and the constraints on its properties.
data RelationshipElementType =
  RelationshipElementType {
    -- | The relationship type
    relationshipElementTypeType :: RelationshipType,
    -- | The label of the start node
    relationshipElementTypeStartLabel :: NodeLabel,
    -- | The label of the end node
    relationshipElementTypeEndLabel :: NodeLabel,
    -- | The constraints on relationships of this type
    relationshipElementTypeConstraints :: [ConstraintDefinition]}
  deriving (Eq, Ord, Read, Show)
_RelationshipElementType = Core.Name "hydra.neo4j.model.RelationshipElementType"
_RelationshipElementType_type = Core.Name "type"
_RelationshipElementType_startLabel = Core.Name "startLabel"
_RelationshipElementType_endLabel = Core.Name "endLabel"
_RelationshipElementType_constraints = Core.Name "constraints"
-- | The type of a relationship. A relationship has exactly one type.
newtype RelationshipType =
  RelationshipType {
    unRelationshipType :: String}
  deriving (Eq, Ord, Read, Show)
_RelationshipType = Core.Name "hydra.neo4j.model.RelationshipType"
-- | One step of a path: a relationship and the node it arrives at. The segment's implicit start is the previous segment's end node, or the path's start node for the first segment, so it is not stored here.
data Segment =
  Segment {
    -- | The relationship traversed in this segment
    segmentRelationship :: Relationship,
    -- | The node this segment arrives at
    segmentEnd :: Node}
  deriving (Eq, Ord, Read, Show)
_Segment = Core.Name "hydra.neo4j.model.Segment"
_Segment_relationship = Core.Name "relationship"
_Segment_end = Core.Name "end"
-- | A spatial reference identifier (SRID): an integer naming the coordinate reference system in which a point's coordinates are expressed. Neo4j supports four: WGS-84 (4326), WGS-84 3D (4979), Cartesian (7203), and Cartesian 3D (9157).
newtype SpatialReferenceIdentifier =
  SpatialReferenceIdentifier {
    unSpatialReferenceIdentifier :: Int}
  deriving (Eq, Ord, Read, Show)
_SpatialReferenceIdentifier = Core.Name "hydra.neo4j.model.SpatialReferenceIdentifier"
-- | A Neo4j value: the general container for instance data, analogous to Hydra's Term. It can represent anything the driver's Value can be (the full TypeSystem), including structural and result-shaped values such as maps, nodes, relationships, paths, and null. Which values are valid as a stored property is a schema/validation concern, not a restriction on Value itself.
data Value =
  -- | A boolean value
  ValueBoolean Bool |
  -- | A byte array
  ValueBytes B.ByteString |
  -- | A date
  ValueDate LocalDate |
  -- | A date and time with a zone
  ValueDateTime ZonedDateTime |
  -- | A temporal amount
  ValueDuration IsoDuration |
  -- | A 64-bit floating-point number
  ValueFloat Double |
  -- | A 64-bit integer
  ValueInteger I.Int64 |
  -- | A list of values
  ValueList [Value] |
  -- | A date and time without a zone
  ValueLocalDateTime LocalDateTime |
  -- | A time of day without a zone
  ValueLocalTime LocalTime |
  -- | A map from keys to values
  ValueMap (M.Map Key Value) |
  -- | A node
  ValueNode Node |
  -- | A null value
  ValueNull |
  -- | A path
  ValuePath Path |
  -- | A spatial point
  ValuePoint Point |
  -- | A relationship
  ValueRelationship Relationship |
  -- | A string value
  ValueString String |
  -- | A time of day with a UTC offset
  ValueTime OffsetTime
  deriving (Eq, Ord, Read, Show)
_Value = Core.Name "hydra.neo4j.model.Value"
_Value_boolean = Core.Name "boolean"
_Value_bytes = Core.Name "bytes"
_Value_date = Core.Name "date"
_Value_dateTime = Core.Name "dateTime"
_Value_duration = Core.Name "duration"
_Value_float = Core.Name "float"
_Value_integer = Core.Name "integer"
_Value_list = Core.Name "list"
_Value_localDateTime = Core.Name "localDateTime"
_Value_localTime = Core.Name "localTime"
_Value_map = Core.Name "map"
_Value_node = Core.Name "node"
_Value_null = Core.Name "null"
_Value_path = Core.Name "path"
_Value_point = Core.Name "point"
_Value_relationship = Core.Name "relationship"
_Value_string = Core.Name "string"
_Value_time = Core.Name "time"
-- | A Neo4j property type, as used in property type constraints and graph type property declarations (the IS :: <TYPE> expression language). Atoms mirror the scalar, temporal, and spatial kinds of Value; lists, vectors, and closed unions compose them.
data ValueType =
  -- | The BOOLEAN type
  ValueTypeBoolean |
  -- | The STRING type
  ValueTypeString |
  -- | The INTEGER type
  ValueTypeInteger |
  -- | The FLOAT type
  ValueTypeFloat |
  -- | The DATE type
  ValueTypeDate |
  -- | The LOCAL TIME type
  ValueTypeLocalTime |
  -- | The ZONED TIME type
  ValueTypeZonedTime |
  -- | The LOCAL DATETIME type
  ValueTypeLocalDateTime |
  -- | The ZONED DATETIME type
  ValueTypeZonedDateTime |
  -- | The DURATION type
  ValueTypeDuration |
  -- | The POINT type
  ValueTypePoint |
  -- | A LIST type, with the given element type
  ValueTypeList ValueType |
  -- | A VECTOR type, with an element type and a dimension
  ValueTypeVector VectorType |
  -- | A closed union of the given types
  ValueTypeUnion [ValueType]
  deriving (Eq, Ord, Read, Show)
_ValueType = Core.Name "hydra.neo4j.model.ValueType"
_ValueType_boolean = Core.Name "boolean"
_ValueType_string = Core.Name "string"
_ValueType_integer = Core.Name "integer"
_ValueType_float = Core.Name "float"
_ValueType_date = Core.Name "date"
_ValueType_localTime = Core.Name "localTime"
_ValueType_zonedTime = Core.Name "zonedTime"
_ValueType_localDateTime = Core.Name "localDateTime"
_ValueType_zonedDateTime = Core.Name "zonedDateTime"
_ValueType_duration = Core.Name "duration"
_ValueType_point = Core.Name "point"
_ValueType_list = Core.Name "list"
_ValueType_vector = Core.Name "vector"
_ValueType_union = Core.Name "union"
-- | A Neo4j VECTOR type: an element type and a fixed dimension.
data VectorType =
  VectorType {
    -- | The element type of the vector
    vectorTypeElement :: ValueType,
    -- | The dimension (length) of the vector
    vectorTypeDimension :: Int}
  deriving (Eq, Ord, Read, Show)
_VectorType = Core.Name "hydra.neo4j.model.VectorType"
_VectorType_element = Core.Name "element"
_VectorType_dimension = Core.Name "dimension"
-- | A time-zone identifier, following java.time.ZoneId: an IANA region id such as "Europe/Paris" (it may also be a fixed-offset string).
newtype ZoneId =
  ZoneId {
    unZoneId :: String}
  deriving (Eq, Ord, Read, Show)
_ZoneId = Core.Name "hydra.neo4j.model.ZoneId"
-- | An offset from UTC, following java.time.ZoneOffset, expressed as a total number of seconds (for example, +01:00 is 3600).
newtype ZoneOffset =
  ZoneOffset {
    unZoneOffset :: Int}
  deriving (Eq, Ord, Read, Show)
_ZoneOffset = Core.Name "hydra.neo4j.model.ZoneOffset"
-- | A date and time with a time zone, following java.time.ZonedDateTime (the driver's asZonedDateTime()). It keeps both the resolved offset and the zone: the zone determines the offset rules, while the offset is the concrete UTC delta at this instant.
data ZonedDateTime =
  ZonedDateTime {
    -- | The local date and time
    zonedDateTimeDateTime :: LocalDateTime,
    -- | The resolved offset from UTC at this instant
    zonedDateTimeOffset :: ZoneOffset,
    -- | The time zone
    zonedDateTimeZone :: ZoneId}
  deriving (Eq, Ord, Read, Show)
_ZonedDateTime = Core.Name "hydra.neo4j.model.ZonedDateTime"
_ZonedDateTime_dateTime = Core.Name "dateTime"
_ZonedDateTime_offset = Core.Name "offset"
_ZonedDateTime_zone = Core.Name "zone"
