module Hydra.Ext.Azure.Dtld where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- A Command describes a function or operation that can be performed on any digital twin.
data Command 
  = Command {
    commandType :: Iri,
    commandName :: String,
    commandId :: (Maybe Dtmi),
    commandComment :: (Maybe String),
    commandDescription :: (Maybe String),
    commandDisplayName :: (Maybe String),
    commandCommandType :: (Maybe CommandType),
    commandRequest :: (Maybe CommandPayload),
    commandResponse :: (Maybe CommandPayload)}
  deriving (Eq, Ord, Read, Show)

_Command = (Core.Name "hydra/ext/azure/dtld.Command")

_Command_type = (Core.FieldName "type")

_Command_name = (Core.FieldName "name")

_Command_id = (Core.FieldName "id")

_Command_comment = (Core.FieldName "comment")

_Command_description = (Core.FieldName "description")

_Command_displayName = (Core.FieldName "displayName")

_Command_commandType = (Core.FieldName "commandType")

_Command_request = (Core.FieldName "request")

_Command_response = (Core.FieldName "response")

-- A CommandPayload describes the inputs to or the outputs from a Command.
data CommandPayload 
  = CommandPayload {
    commandPayloadName :: String,
    commandPayloadSchema :: Schema,
    commandPayloadId :: (Maybe Dtmi),
    commandPayloadComment :: (Maybe String),
    commandPayloadDescription :: (Maybe String),
    commandPayloadDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_CommandPayload = (Core.Name "hydra/ext/azure/dtld.CommandPayload")

_CommandPayload_name = (Core.FieldName "name")

_CommandPayload_schema = (Core.FieldName "schema")

_CommandPayload_id = (Core.FieldName "id")

_CommandPayload_comment = (Core.FieldName "comment")

_CommandPayload_description = (Core.FieldName "description")

_CommandPayload_displayName = (Core.FieldName "displayName")

-- CommandType is deprecated. Either value, synchronous or asynchronous, has the same meaning: a command that starts execution within a configurable time and that completes execution within a configurable time.
data CommandType 
  = CommandTypeSynchronous 
  | CommandTypeAsynchronous 
  deriving (Eq, Ord, Read, Show)

_CommandType = (Core.Name "hydra/ext/azure/dtld.CommandType")

_CommandType_synchronous = (Core.FieldName "synchronous")

_CommandType_asynchronous = (Core.FieldName "asynchronous")

-- Components enable interfaces to be composed of other interfaces. Components are different from relationships because they describe contents that are directly part of the interface. (A relationship describes a link between two interfaces.)
data Component 
  = Component {
    componentType :: Iri,
    componentName :: String,
    componentSchema :: Interface,
    componentId :: (Maybe Dtmi),
    componentComment :: (Maybe String),
    componentDescription :: (Maybe String),
    componentDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Component = (Core.Name "hydra/ext/azure/dtld.Component")

_Component_type = (Core.FieldName "type")

_Component_name = (Core.FieldName "name")

_Component_schema = (Core.FieldName "schema")

_Component_id = (Core.FieldName "id")

_Component_comment = (Core.FieldName "comment")

_Component_description = (Core.FieldName "description")

_Component_displayName = (Core.FieldName "displayName")

-- A digital twin model identifier
newtype Dtmi 
  = Dtmi {
    unDtmi :: String}
  deriving (Eq, Ord, Read, Show)

_Dtmi = (Core.Name "hydra/ext/azure/dtld.Dtmi")

-- An EnumValue describes an element of an Enum.
data EnumValue 
  = EnumValue {
    enumValueName :: String,
    enumValueEnumValue :: IntegerOrString,
    enumValueId :: (Maybe Dtmi),
    enumValueComment :: (Maybe String),
    enumValueDescription :: (Maybe String),
    enumValueDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_EnumValue = (Core.Name "hydra/ext/azure/dtld.EnumValue")

_EnumValue_name = (Core.FieldName "name")

_EnumValue_enumValue = (Core.FieldName "enumValue")

_EnumValue_id = (Core.FieldName "id")

_EnumValue_comment = (Core.FieldName "comment")

_EnumValue_description = (Core.FieldName "description")

_EnumValue_displayName = (Core.FieldName "displayName")

-- A Field describes a field in an Object.
data Field 
  = Field {
    fieldName :: String,
    fieldSchema :: Schema,
    fieldId :: (Maybe Dtmi),
    fieldComment :: (Maybe String),
    fieldDescription :: (Maybe String),
    fieldDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra/ext/azure/dtld.Field")

_Field_name = (Core.FieldName "name")

_Field_schema = (Core.FieldName "schema")

_Field_id = (Core.FieldName "id")

_Field_comment = (Core.FieldName "comment")

_Field_description = (Core.FieldName "description")

_Field_displayName = (Core.FieldName "displayName")

data IntegerOrString 
  = IntegerOrStringInteger Int
  | IntegerOrStringString String
  deriving (Eq, Ord, Read, Show)

_IntegerOrString = (Core.Name "hydra/ext/azure/dtld.IntegerOrString")

_IntegerOrString_integer = (Core.FieldName "integer")

_IntegerOrString_string = (Core.FieldName "string")

data Interface 
  = Interface {
    interfaceId :: Dtmi,
    interfaceType :: Iri,
    interfaceContext :: Iri,
    interfaceComment :: (Maybe String),
    interfaceContents :: (Maybe (Set Interface_Contents)),
    interfaceDescription :: (Maybe String),
    interfaceDisplayName :: (Maybe String),
    interfaceExtends :: (Maybe (Set Interface)),
    interfaceSchemas :: (Maybe (Set Schema_Interface))}
  deriving (Eq, Ord, Read, Show)

_Interface = (Core.Name "hydra/ext/azure/dtld.Interface")

_Interface_id = (Core.FieldName "id")

_Interface_type = (Core.FieldName "type")

_Interface_context = (Core.FieldName "context")

_Interface_comment = (Core.FieldName "comment")

_Interface_contents = (Core.FieldName "contents")

_Interface_description = (Core.FieldName "description")

_Interface_displayName = (Core.FieldName "displayName")

_Interface_extends = (Core.FieldName "extends")

_Interface_schemas = (Core.FieldName "schemas")

data Interface_Contents 
  = Interface_ContentsCommand Command
  | Interface_ContentsComponent Component
  | Interface_ContentsProperty Property
  | Interface_ContentsRelationship Relationship
  | Interface_ContentsTelemetry Telemetry
  deriving (Eq, Ord, Read, Show)

_Interface_Contents = (Core.Name "hydra/ext/azure/dtld.Interface.Contents")

_Interface_Contents_command = (Core.FieldName "command")

_Interface_Contents_component = (Core.FieldName "component")

_Interface_Contents_property = (Core.FieldName "property")

_Interface_Contents_relationship = (Core.FieldName "relationship")

_Interface_Contents_telemetry = (Core.FieldName "telemetry")

newtype Iri 
  = Iri {
    unIri :: String}
  deriving (Eq, Ord, Read, Show)

_Iri = (Core.Name "hydra/ext/azure/dtld.Iri")

-- A MapKey describes the key in a Map. The schema of a MapKey must be string.
data MapKey 
  = MapKey {
    mapKeyName :: String,
    mapKeySchema :: Schema,
    mapKeyId :: (Maybe Dtmi),
    mapKeyComment :: (Maybe String),
    mapKeyDescription :: (Maybe String),
    mapKeyDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_MapKey = (Core.Name "hydra/ext/azure/dtld.MapKey")

_MapKey_name = (Core.FieldName "name")

_MapKey_schema = (Core.FieldName "schema")

_MapKey_id = (Core.FieldName "id")

_MapKey_comment = (Core.FieldName "comment")

_MapKey_description = (Core.FieldName "description")

_MapKey_displayName = (Core.FieldName "displayName")

-- A MapValue describes the values in a Map.
data MapValue 
  = MapValue {
    mapValueName :: String,
    mapValueSchema :: Schema,
    mapValueId :: (Maybe Dtmi),
    mapValueComment :: (Maybe String),
    mapValueDescription :: (Maybe String),
    mapValueDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_MapValue = (Core.Name "hydra/ext/azure/dtld.MapValue")

_MapValue_name = (Core.FieldName "name")

_MapValue_schema = (Core.FieldName "schema")

_MapValue_id = (Core.FieldName "id")

_MapValue_comment = (Core.FieldName "comment")

_MapValue_description = (Core.FieldName "description")

_MapValue_displayName = (Core.FieldName "displayName")

-- A Property describes the read-only and read/write state of any digital twin. For example, a device serial number may be a read-only property, the desired temperature on a thermostat may be a read-write property; and the name of a room may be a read-write property.
data Property 
  = Property {
    propertyType :: Iri,
    propertyName :: String,
    propertySchema :: Schema,
    propertyId :: (Maybe Dtmi),
    propertyComment :: (Maybe String),
    propertyDescription :: (Maybe String),
    propertyDisplayName :: (Maybe String),
    propertyUnit :: (Maybe Unit),
    propertyWritable :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/ext/azure/dtld.Property")

_Property_type = (Core.FieldName "type")

_Property_name = (Core.FieldName "name")

_Property_schema = (Core.FieldName "schema")

_Property_id = (Core.FieldName "id")

_Property_comment = (Core.FieldName "comment")

_Property_description = (Core.FieldName "description")

_Property_displayName = (Core.FieldName "displayName")

_Property_unit = (Core.FieldName "unit")

_Property_writable = (Core.FieldName "writable")

-- A Relationship describes a link to another digital twin and enables graphs of digital twins to be created. Relationships are different from Components because they describe a link to a separate digital twin.
data Relationship 
  = Relationship {
    relationshipType :: Iri,
    relationshipName :: String,
    relationshipId :: (Maybe Dtmi),
    relationshipComment :: (Maybe String),
    relationshipDescription :: (Maybe String),
    relationshipDisplayName :: (Maybe String),
    relationshipMaxMultiplicity :: (Maybe Int),
    relationshipMinMultiplicity :: (Maybe Int),
    relationshipProperties :: (Maybe (Set Property)),
    relationshipTarget :: (Maybe Interface),
    relationshipWritable :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_Relationship = (Core.Name "hydra/ext/azure/dtld.Relationship")

_Relationship_type = (Core.FieldName "type")

_Relationship_name = (Core.FieldName "name")

_Relationship_id = (Core.FieldName "id")

_Relationship_comment = (Core.FieldName "comment")

_Relationship_description = (Core.FieldName "description")

_Relationship_displayName = (Core.FieldName "displayName")

_Relationship_maxMultiplicity = (Core.FieldName "maxMultiplicity")

_Relationship_minMultiplicity = (Core.FieldName "minMultiplicity")

_Relationship_properties = (Core.FieldName "properties")

_Relationship_target = (Core.FieldName "target")

_Relationship_writable = (Core.FieldName "writable")

-- Schemas are used to describe the on-the-wire or serialized format of the data in a digital twin interface. A full set of primitive data types are provided, along with support for a variety of complex schemas in the forms of Arrays, Enums, Maps, and Objects. Schemas described through digital twin's schema definition language are compatible with popular serialization formats, including JSON, Avro, and Protobuf.
data Schema 
  = SchemaPrimitive Schema_Primitive
  | SchemaComplex Schema_Complex
  deriving (Eq, Ord, Read, Show)

_Schema = (Core.Name "hydra/ext/azure/dtld.Schema")

_Schema_primitive = (Core.FieldName "primitive")

_Schema_complex = (Core.FieldName "complex")

-- An Array describes an indexable data type where each element is of the same schema. An Array elements' schema can itself be a primitive or complex schema.
data Schema_Array 
  = Schema_Array {
    schema_ArrayType :: Iri,
    schema_ArrayElementSchema :: Schema,
    schema_ArrayId :: (Maybe Dtmi),
    schema_ArrayComment :: (Maybe String),
    schema_ArrayDescription :: (Maybe String),
    schema_ArrayDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Schema_Array = (Core.Name "hydra/ext/azure/dtld.Schema.Array")

_Schema_Array_type = (Core.FieldName "type")

_Schema_Array_elementSchema = (Core.FieldName "elementSchema")

_Schema_Array_id = (Core.FieldName "id")

_Schema_Array_comment = (Core.FieldName "comment")

_Schema_Array_description = (Core.FieldName "description")

_Schema_Array_displayName = (Core.FieldName "displayName")

-- Complex schemas are designed for supporting complex data types made up of primitive data types. Currently the following complex schemas are provided: Array, Enum, Map, and Object. A complex schema can be specified directly as the value in a schema statement or described in the interface schemas set and referenced in the schema statement.
data Schema_Complex 
  = Schema_ComplexArray Schema_Array
  | Schema_ComplexEnum Schema_Enum
  | Schema_ComplexMap Schema_Map
  | Schema_ComplexObject Schema_Object
  deriving (Eq, Ord, Read, Show)

_Schema_Complex = (Core.Name "hydra/ext/azure/dtld.Schema.Complex")

_Schema_Complex_array = (Core.FieldName "array")

_Schema_Complex_enum = (Core.FieldName "enum")

_Schema_Complex_map = (Core.FieldName "map")

_Schema_Complex_object = (Core.FieldName "object")

-- An Enum describes a data type with a set of named labels that map to values. The values in an Enum can be either integers or strings, but the labels are always strings.
data Schema_Enum 
  = Schema_Enum {
    schema_EnumType :: Iri,
    schema_EnumEnumValues :: [EnumValue],
    schema_EnumValueSchema :: IntegerOrString,
    schema_EnumId :: (Maybe Dtmi),
    schema_EnumComment :: (Maybe String),
    schema_EnumDescription :: (Maybe String),
    schema_EnumDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Schema_Enum = (Core.Name "hydra/ext/azure/dtld.Schema.Enum")

_Schema_Enum_type = (Core.FieldName "type")

_Schema_Enum_enumValues = (Core.FieldName "enumValues")

_Schema_Enum_valueSchema = (Core.FieldName "valueSchema")

_Schema_Enum_id = (Core.FieldName "id")

_Schema_Enum_comment = (Core.FieldName "comment")

_Schema_Enum_description = (Core.FieldName "description")

_Schema_Enum_displayName = (Core.FieldName "displayName")

-- Within an interface definition, complex schemas may be defined for reusability across Telemetry, Properties, and Commands. This is designed to promote readability and improved maintenance because schemas that are reused can be defined once (per interface). Interface schemas are defined in the schemas property of an interface.
data Schema_Interface 
  = Schema_Interface {
    schema_InterfaceId :: Dtmi,
    schema_InterfaceType :: Schema_Interface_Type,
    schema_InterfaceComment :: (Maybe String),
    schema_InterfaceDescription :: (Maybe String),
    schema_InterfaceDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Schema_Interface = (Core.Name "hydra/ext/azure/dtld.Schema.Interface")

_Schema_Interface_id = (Core.FieldName "id")

_Schema_Interface_type = (Core.FieldName "type")

_Schema_Interface_comment = (Core.FieldName "comment")

_Schema_Interface_description = (Core.FieldName "description")

_Schema_Interface_displayName = (Core.FieldName "displayName")

data Schema_Interface_Type 
  = Schema_Interface_TypeArray Schema_Array
  | Schema_Interface_TypeEnum Schema_Enum
  | Schema_Interface_TypeMap Schema_Map
  | Schema_Interface_TypeObject Schema_Object
  deriving (Eq, Ord, Read, Show)

_Schema_Interface_Type = (Core.Name "hydra/ext/azure/dtld.Schema.Interface.Type")

_Schema_Interface_Type_array = (Core.FieldName "array")

_Schema_Interface_Type_enum = (Core.FieldName "enum")

_Schema_Interface_Type_map = (Core.FieldName "map")

_Schema_Interface_Type_object = (Core.FieldName "object")

-- A Map describes a data type of key-value pairs where the values share the same schema. The key in a Map must be a string. The values in a Map can be any schema.
data Schema_Map 
  = Schema_Map {
    schema_MapType :: Iri,
    schema_MapMapKey :: MapKey,
    schema_MapMapValue :: MapValue,
    schema_MapId :: (Maybe Dtmi),
    schema_MapComment :: (Maybe String),
    schema_MapDescription :: (Maybe String),
    schema_MapDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Schema_Map = (Core.Name "hydra/ext/azure/dtld.Schema.Map")

_Schema_Map_type = (Core.FieldName "type")

_Schema_Map_mapKey = (Core.FieldName "mapKey")

_Schema_Map_mapValue = (Core.FieldName "mapValue")

_Schema_Map_id = (Core.FieldName "id")

_Schema_Map_comment = (Core.FieldName "comment")

_Schema_Map_description = (Core.FieldName "description")

_Schema_Map_displayName = (Core.FieldName "displayName")

-- An Object describes a data type made up of named fields (like a struct in C). The fields in an Object map can be primitive or complex schemas.
data Schema_Object 
  = Schema_Object {
    schema_ObjectType :: Iri,
    schema_ObjectFields :: (Set Field),
    schema_ObjectId :: (Maybe Dtmi),
    schema_ObjectComment :: (Maybe String),
    schema_ObjectDescription :: (Maybe String),
    schema_ObjectDisplayName :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Schema_Object = (Core.Name "hydra/ext/azure/dtld.Schema.Object")

_Schema_Object_type = (Core.FieldName "type")

_Schema_Object_fields = (Core.FieldName "fields")

_Schema_Object_id = (Core.FieldName "id")

_Schema_Object_comment = (Core.FieldName "comment")

_Schema_Object_description = (Core.FieldName "description")

_Schema_Object_displayName = (Core.FieldName "displayName")

-- A full set of primitive data types are provided and can be specified directly as the value in a schema statement in a digital twin interface.
data Schema_Primitive 
  = Schema_PrimitiveBoolean 
  | Schema_PrimitiveDate 
  | Schema_PrimitiveDateTime 
  | Schema_PrimitiveDouble 
  | Schema_PrimitiveDuration 
  | Schema_PrimitiveFloat 
  | Schema_PrimitiveInteger 
  | Schema_PrimitiveLong 
  | Schema_PrimitiveString 
  | Schema_PrimitiveTime 
  deriving (Eq, Ord, Read, Show)

_Schema_Primitive = (Core.Name "hydra/ext/azure/dtld.Schema.Primitive")

_Schema_Primitive_boolean = (Core.FieldName "boolean")

_Schema_Primitive_date = (Core.FieldName "date")

_Schema_Primitive_dateTime = (Core.FieldName "dateTime")

_Schema_Primitive_double = (Core.FieldName "double")

_Schema_Primitive_duration = (Core.FieldName "duration")

_Schema_Primitive_float = (Core.FieldName "float")

_Schema_Primitive_integer = (Core.FieldName "integer")

_Schema_Primitive_long = (Core.FieldName "long")

_Schema_Primitive_string = (Core.FieldName "string")

_Schema_Primitive_time = (Core.FieldName "time")

-- Telemetry describes the data emitted by any digital twin, whether the data is a regular stream of sensor readings or a computed stream of data, such as occupancy, or an occasional error or information message.
data Telemetry 
  = Telemetry {
    telemetryType :: Iri,
    telemetryName :: String,
    telemetrySchema :: Schema,
    telemetryId :: (Maybe Dtmi),
    telemetryComment :: (Maybe String),
    telemetryDescription :: (Maybe String),
    telemetryDisplayName :: (Maybe String),
    telemetryUnit :: (Maybe Unit)}
  deriving (Eq, Ord, Read, Show)

_Telemetry = (Core.Name "hydra/ext/azure/dtld.Telemetry")

_Telemetry_type = (Core.FieldName "type")

_Telemetry_name = (Core.FieldName "name")

_Telemetry_schema = (Core.FieldName "schema")

_Telemetry_id = (Core.FieldName "id")

_Telemetry_comment = (Core.FieldName "comment")

_Telemetry_description = (Core.FieldName "description")

_Telemetry_displayName = (Core.FieldName "displayName")

_Telemetry_unit = (Core.FieldName "unit")

data Unit 
  = Unit {}
  deriving (Eq, Ord, Read, Show)

_Unit = (Core.Name "hydra/ext/azure/dtld.Unit")