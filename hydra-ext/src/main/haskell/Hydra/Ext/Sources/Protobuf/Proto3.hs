module Hydra.Ext.Sources.Protobuf.Proto3 where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.protobuf.proto3"

define :: String -> Type -> Binding
define = defineType ns

proto3 :: String -> Type
proto3 = typeref ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just ("A model for Protocol Buffers v3 enum and message types, designed as a target for transformations."
      ++ "This model is loosely based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/type.proto,"
      ++ " as well as the proto3 reference documentation")
  where
    elements = [
      definition,
      enumDefinition,
      enumValue,
      enumValueName,
      field,
      fieldName_,
      fieldType,
      fileReference,
      mapType,
      messageDefinition,
      option,
      packageName,
      protoFile,
      scalarType,
      simpleType,
      typeName,
      typeReference,
      value]

definition :: Binding
definition = define "Definition" $
  T.union [
    "enum">: proto3 "EnumDefinition",
    "message">: proto3 "MessageDefinition"]

enumDefinition :: Binding
enumDefinition = define "EnumDefinition" $
  doc "Enum type definition" $
  T.record [
    "name">:
      doc "Enum type name" $
      proto3 "TypeName",
    "values">:
      doc "Enum value definitions" $
      T.list $ proto3 "EnumValue",
    "options">:
      doc "Protocol buffer options" $
      T.list $ proto3 "Option"]

enumValue :: Binding
enumValue = define "EnumValue" $
  doc "Enum value definition" $
  T.record [
    "name">:
      doc "Enum value name" $
      proto3 "EnumValueName",
    "number">:
      doc "Enum value number"
      T.int32,
    "options">:
      doc "Protocol buffer options" $
      T.list $ proto3 "Option"]

enumValueName :: Binding
enumValueName = define "EnumValueName" $
  T.wrap T.string

field :: Binding
field = define "Field" $
  doc "A single field of a message type" $
  T.record [
    "name">:
      doc "The field name" $
      proto3 "FieldName",
    "jsonName">:
      doc "The field JSON name" $
      T.optional T.string,
    "type">:
      doc "The datatype of the field" $
      proto3 "FieldType",
    "number">:
      doc "The field number"
      T.int32,
    "options">:
      doc "The protocol buffer options" $
      T.list $ proto3 "Option"]

fieldName_ :: Binding
fieldName_ = define "FieldName" $
  doc "The name of a field" $
  T.wrap T.string

fieldType :: Binding
fieldType = define "FieldType" $
  T.union [
    "map">: proto3 "MapType",
    "oneof">: T.list $ proto3 "Field",
    "repeated">: proto3 "SimpleType",
    "simple">: proto3 "SimpleType"]

fileReference :: Binding
fileReference = define "FileReference" $
  T.wrap T.string

mapType :: Binding
mapType = define "MapType" $
  T.record [
    "keys">: proto3 "SimpleType",
    "values">: proto3 "SimpleType"]

messageDefinition :: Binding
messageDefinition = define "MessageDefinition" $
  doc "A protocol buffer message type" $
  T.record [
    "name">:
      doc "The fully qualified message name" $
      proto3 "TypeName",
    "fields">:
      doc "The list of fields" $
      T.list $ proto3 "Field",
    "options">:
      doc "The protocol buffer options" $
      T.list $ proto3 "Option"]

option :: Binding
option = define "Option" $
  doc ("A protocol buffer option, which can be attached to a message, field, " ++
       "enumeration, etc") $
  T.record [
    "name">:
      doc ("The option's name. For protobuf built-in options (options defined in " ++
           "descriptor.proto), this is the short name. For example, `\"map_entry\"`. " ++
           "For custom options, it should be the fully-qualified name. For example, " ++
           "`\"google.api.http\"`.")
      T.string,
    "value">:
      doc ("The option's value") $
      proto3 "Value"]

packageName :: Binding
packageName = define "PackageName" $
  T.wrap T.string

protoFile :: Binding
protoFile = define "ProtoFile" $
  doc "A .proto file, usually containing one or more enum or message type definitions" $
  T.record [
    "package">: proto3 "PackageName",
    "imports">: T.list $ proto3 "FileReference",
    "types">: T.list $ proto3 "Definition",
    "options">: T.list $ proto3 "Option"]

scalarType :: Binding
scalarType = define "ScalarType" $
  doc "One of several Proto3 scalar types" $
  T.enum [
    "bool",
    "bytes",
    "double",
    "fixed32",
    "fixed64",
    "float",
    "int32",
    "int64",
    "sfixed32",
    "sfixed64",
    "sint32",
    "sint64",
    "string",
    "uint32",
    "uint64"]

simpleType :: Binding
simpleType = define "SimpleType" $
  doc "A scalar type or a reference to an enum type or message type" $
  T.union [
    "reference">: proto3 "TypeName",
    "scalar">: proto3 "ScalarType"]

typeName :: Binding
typeName = define "TypeName" $
  doc "The local name of an enum type or message type" $
  T.wrap T.string

typeReference :: Binding
typeReference = define "TypeReference" $
  doc "A reference to an enum type or message type" $
  T.wrap T.string

value :: Binding
value = define "Value" $
  doc "A scalar value" $
  T.union [
    "boolean">: T.boolean,
    "string">: T.string
    -- Add other scalar value types as needed
  ]
