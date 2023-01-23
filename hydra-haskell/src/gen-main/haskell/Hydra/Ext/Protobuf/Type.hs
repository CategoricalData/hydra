-- | Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/type.proto

module Hydra.Ext.Protobuf.Type where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Protobuf.Any as Any
import qualified Hydra.Ext.Protobuf.SourceContext as SourceContext
import Data.List
import Data.Map
import Data.Set

-- | A protocol buffer message type.
data Type = 
  Type {
    -- | The fully qualified message name.
    typeName :: String,
    -- | The list of fields.
    typeFields :: [Field],
    -- | The list of types appearing in `oneof` definitions in this type.
    typeOneofs :: [String],
    -- | The protocol buffer options.
    typeOptions :: [Option],
    -- | The source context.
    typeSourceContext :: SourceContext.SourceContext,
    -- | The source syntax.
    typeSyntax :: Syntax}
  deriving (Eq, Ord, Read, Show)

_Type = (Core.Name "hydra/ext/protobuf/type.Type")

_Type_name = (Core.FieldName "name")

_Type_fields = (Core.FieldName "fields")

_Type_oneofs = (Core.FieldName "oneofs")

_Type_options = (Core.FieldName "options")

_Type_sourceContext = (Core.FieldName "sourceContext")

_Type_syntax = (Core.FieldName "syntax")

-- | Basic field types.
data Kind = 
  -- | Field type unknown.
  KindUnknown  |
  -- | Field type double.
  KindDouble  |
  -- | Field type float.
  KindFloat  |
  -- | Field type int64.
  KindInt64  |
  -- | Field type uint64.
  KindUint64  |
  -- | Field type int32.
  KindInt32  |
  -- | Field type fixed64.
  KindFixed64  |
  -- | Field type fixed32.
  KindFixed32  |
  -- | Field type bool.
  KindBool  |
  -- | Field type string.
  KindString  |
  -- | Field type group. Proto2 syntax only, and deprecated.
  KindGroup  |
  -- | Field type message.
  KindMessage  |
  -- | Field type bytes.
  KindBytes  |
  -- | Field type uint32.
  KindUint32  |
  -- | Field type enum.
  KindEnum  |
  -- | Field type sfixed32.
  KindSfixed32  |
  -- | Field type sfixed64.
  KindSfixed64  |
  -- | Field type sint32.
  KindSint32  |
  -- | Field type sint64.
  KindSint64 
  deriving (Eq, Ord, Read, Show)

_Kind = (Core.Name "hydra/ext/protobuf/type.Kind")

_Kind_unknown = (Core.FieldName "unknown")

_Kind_double = (Core.FieldName "double")

_Kind_float = (Core.FieldName "float")

_Kind_int64 = (Core.FieldName "int64")

_Kind_uint64 = (Core.FieldName "uint64")

_Kind_int32 = (Core.FieldName "int32")

_Kind_fixed64 = (Core.FieldName "fixed64")

_Kind_fixed32 = (Core.FieldName "fixed32")

_Kind_bool = (Core.FieldName "bool")

_Kind_string = (Core.FieldName "string")

_Kind_group = (Core.FieldName "group")

_Kind_message = (Core.FieldName "message")

_Kind_bytes = (Core.FieldName "bytes")

_Kind_uint32 = (Core.FieldName "uint32")

_Kind_enum = (Core.FieldName "enum")

_Kind_sfixed32 = (Core.FieldName "sfixed32")

_Kind_sfixed64 = (Core.FieldName "sfixed64")

_Kind_sint32 = (Core.FieldName "sint32")

_Kind_sint64 = (Core.FieldName "sint64")

-- | Whether a field is optional, required, or repeated.
data Cardinality = 
  -- | For fields with unknown cardinality.
  CardinalityUnknown  |
  -- | For optional fields.
  CardinalityOptional  |
  -- | For required fields. Proto2 syntax only.
  CardinalityRequired  |
  -- | For repeated fields.
  CardinalityRepeated 
  deriving (Eq, Ord, Read, Show)

_Cardinality = (Core.Name "hydra/ext/protobuf/type.Cardinality")

_Cardinality_unknown = (Core.FieldName "unknown")

_Cardinality_optional = (Core.FieldName "optional")

_Cardinality_required = (Core.FieldName "required")

_Cardinality_repeated = (Core.FieldName "repeated")

-- | A single field of a message type.
data Field = 
  -- | The field type.
  FieldKind Kind |
  -- | The field cardinality.
  FieldCardinality Cardinality |
  -- | The field number.
  FieldNumber Int |
  -- | The field name.
  FieldName String |
  -- | The field type URL, without the scheme, for message or enumeration types. Example: `"type.googleapis.com/google.protobuf.Timestamp"`.
  FieldTypeUrl String |
  -- | The index of the field type in `Type.oneofs`, for message or enumeration types. The first type has index 1; zero means the type is not in the list.
  FieldOneofIndex Int |
  -- | Whether to use alternative packed wire representation.
  FieldPacked Bool |
  -- | The protocol buffer options.
  FieldOptions [Option] |
  -- | The field JSON name.
  FieldJsonName String |
  -- | The string value of the default value of this field. Proto2 syntax only.
  FieldDefaultValue String
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra/ext/protobuf/type.Field")

_Field_kind = (Core.FieldName "kind")

_Field_cardinality = (Core.FieldName "cardinality")

_Field_number = (Core.FieldName "number")

_Field_name = (Core.FieldName "name")

_Field_typeUrl = (Core.FieldName "typeUrl")

_Field_oneofIndex = (Core.FieldName "oneofIndex")

_Field_packed = (Core.FieldName "packed")

_Field_options = (Core.FieldName "options")

_Field_jsonName = (Core.FieldName "jsonName")

_Field_defaultValue = (Core.FieldName "defaultValue")

-- | Enum type definition.
data Enum_ = 
  Enum_ {
    -- | Enum type name.
    enumName :: String,
    -- | Enum value definitions.
    enumEnumvalue :: [EnumValue],
    -- | Protocol buffer options.
    enumOptions :: [Option],
    -- | The source context.
    enumSourceContext :: SourceContext.SourceContext,
    -- | The source syntax.
    enumSyntax :: Syntax}
  deriving (Eq, Ord, Read, Show)

_Enum = (Core.Name "hydra/ext/protobuf/type.Enum")

_Enum_name = (Core.FieldName "name")

_Enum_enumvalue = (Core.FieldName "enumvalue")

_Enum_options = (Core.FieldName "options")

_Enum_sourceContext = (Core.FieldName "sourceContext")

_Enum_syntax = (Core.FieldName "syntax")

-- | Enum value definition.
data EnumValue = 
  EnumValue {
    -- | Enum value name.
    enumValueName :: String,
    -- | Enum value number.
    enumValueNumber :: Int,
    -- | Protocol buffer options.
    enumValueOptions :: [Option]}
  deriving (Eq, Ord, Read, Show)

_EnumValue = (Core.Name "hydra/ext/protobuf/type.EnumValue")

_EnumValue_name = (Core.FieldName "name")

_EnumValue_number = (Core.FieldName "number")

_EnumValue_options = (Core.FieldName "options")

-- | A protocol buffer option, which can be attached to a message, field, enumeration, etc.
data Option = 
  Option {
    -- | The option's name. For protobuf built-in options (options defined in descriptor.proto), this is the short name. For example, `"map_entry"`. For custom options, it should be the fully-qualified name. For example, `"google.api.http"`.
    optionName :: String,
    -- | The option's value packed in an Any message. If the value is a primitive, the corresponding wrapper type defined in google/protobuf/wrappers.proto should be used. If the value is an enum, it should be stored as an int32 value using the google.protobuf.Int32Value type.
    optionValue :: Any.Any}
  deriving (Eq, Ord, Read, Show)

_Option = (Core.Name "hydra/ext/protobuf/type.Option")

_Option_name = (Core.FieldName "name")

_Option_value = (Core.FieldName "value")

-- | The syntax in which a protocol buffer element is defined.
data Syntax = 
  -- | Syntax `proto2`.
  SyntaxProto2  |
  -- | Syntax `proto3`.
  SyntaxProto3 
  deriving (Eq, Ord, Read, Show)

_Syntax = (Core.Name "hydra/ext/protobuf/type.Syntax")

_Syntax_proto2 = (Core.FieldName "proto2")

_Syntax_proto3 = (Core.FieldName "proto3")