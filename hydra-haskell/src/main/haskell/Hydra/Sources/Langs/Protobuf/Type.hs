{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Langs.Protobuf.Type where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Sources.Langs.Protobuf.Any
import Hydra.Sources.Langs.Protobuf.SourceContext
import Hydra.Dsl.Types as Types


pbTypeNs = Namespace "hydra/langs/protobuf/type"
pbType = typeref pbTypeNs

-- Note: for now, all fields are considered to be required; some may need to be made optional in the future.
--       For example the "name" of a message type is clearly required, while the syntax and source context
--       should probably be optional.
protobufTypeModule :: Module Kv
protobufTypeModule = Module pbTypeNs elements [protobufAnyModule, protobufSourceContextModule] $
    Just "Based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/type.proto"
  where
    def = datatype pbTypeNs

    elements = [
--  // A protocol buffer message type.
--  message Type {
      def "Type" $
        doc "A protocol buffer message type." $
        record [
--    // The fully qualified message name.
--    string name = 1;
          "name">:
            doc "The fully qualified message name."
            string,
--    // The list of fields.
--    repeated Field fields = 2;
          "fields">:
            doc "The list of fields." $
            list $ pbType "Field",
--    // The list of types appearing in `oneof` definitions in this type.
--    repeated string oneofs = 3;
          "oneofs">:
            doc "The list of types appearing in `oneof` definitions in this type." $
            list string,
--    // The protocol buffer options.
--    repeated Option options = 4;
          "options">:
            doc "The protocol buffer options." $
            list $ pbType "Option",
--    // The source context.
--    SourceContext source_context = 5;
          "sourceContext">:
            doc "The source context." $
            pbSourceContext "SourceContext",
--    // The source syntax.
--    Syntax syntax = 6;
          "syntax">:
            doc "The source syntax." $
            pbType "Syntax"],
--  }


--  note: inner type pulled up out of Field
--    // Basic field types.
--    enum Kind {
      def "Kind" $
        doc "Basic field types." $
        union [
--      // Field type unknown.
--      TYPE_UNKNOWN = 0;
          "unknown">: doc "Field type unknown." unit,
--      // Field type double.
--      TYPE_DOUBLE = 1;
          "double">: doc "Field type double." unit,
--      // Field type float.
--      TYPE_FLOAT = 2;
          "float">: doc "Field type float." unit,
--      // Field type int64.
--      TYPE_INT64 = 3;
          "int64">: doc "Field type int64." unit,
--      // Field type uint64.
--      TYPE_UINT64 = 4;
          "uint64">: doc "Field type uint64." unit,
--      // Field type int32.
--      TYPE_INT32 = 5;
          "int32">: doc "Field type int32." unit,
--      // Field type fixed64.
--      TYPE_FIXED64 = 6;
          "fixed64">: doc "Field type fixed64." unit,
--      // Field type fixed32.
--      TYPE_FIXED32 = 7;
          "fixed32">: doc "Field type fixed32." unit,
--      // Field type bool.
--      TYPE_BOOL = 8;
          "bool">: doc "Field type bool." unit,
--      // Field type string.
--      TYPE_STRING = 9;
          "string">: doc "Field type string." unit,
--      // Field type group. Proto2 syntax only, and deprecated.
--      TYPE_GROUP = 10;
          "group">: doc "Field type group. Proto2 syntax only, and deprecated." unit,
--      // Field type message.
--      TYPE_MESSAGE = 11;
          "message">: doc "Field type message." unit,
--      // Field type bytes.
--      TYPE_BYTES = 12;
          "bytes">: doc "Field type bytes." unit,
--      // Field type uint32.
--      TYPE_UINT32 = 13;
          "uint32">: doc "Field type uint32." unit,
--      // Field type enum.
--      TYPE_ENUM = 14;
          "enum">: doc "Field type enum." unit,
--      // Field type sfixed32.
--      TYPE_SFIXED32 = 15;
          "sfixed32">: doc "Field type sfixed32." unit,
--      // Field type sfixed64.
--      TYPE_SFIXED64 = 16;
          "sfixed64">: doc "Field type sfixed64." unit,
--      // Field type sint32.
--      TYPE_SINT32 = 17;
          "sint32">: doc "Field type sint32." unit,
--      // Field type sint64.
--      TYPE_SINT64 = 18;
          "sint64">: doc "Field type sint64." unit],
--    }

--  note: inner type pulled up out of Field
--    // Whether a field is optional, required, or repeated.
--    enum Cardinality {
      def "Cardinality" $
        doc "Whether a field is optional, required, or repeated." $
        union [
--      // For fields with unknown cardinality.
--      CARDINALITY_UNKNOWN = 0;
          "unknown">: doc "For fields with unknown cardinality." unit,
--      // For optional fields.
--      CARDINALITY_OPTIONAL = 1;
          "optional">: doc "For optional fields." unit,
--      // For required fields. Proto2 syntax only.
--      CARDINALITY_REQUIRED = 2;
          "required">: doc "For required fields. Proto2 syntax only." unit,
--      // For repeated fields.
--      CARDINALITY_REPEATED = 3;
          "repeated">: doc "For repeated fields." unit],
--    }

--  // A single field of a message type.
--  message Field {
      def "Field" $
        doc "A single field of a message type." $
        union [
--    // The field type.
--    Kind kind = 1;
          "kind">:
            doc "The field type." $
            pbType "Kind",
--    // The field cardinality.
--    Cardinality cardinality = 2;
          "cardinality">:
            doc "The field cardinality." $
            pbType "Cardinality",
--    // The field number.
--    int32 number = 3;
          "number">:
            doc "The field number."
            int32,
--    // The field name.
--    string name = 4;
          "name">:
            doc "The field name."
            string,
--    // The field type URL, without the scheme, for message or enumeration
--    // types. Example: `"type.googleapis.com/google.protobuf.Timestamp"`.
--    string type_url = 6;
          "typeUrl">:
            doc ("The field type URL, without the scheme, for message or enumeration " ++
                 "types. Example: `\"type.googleapis.com/google.protobuf.Timestamp\"`.")
            string,
--    // The index of the field type in `Type.oneofs`, for message or enumeration
--    // types. The first type has index 1; zero means the type is not in the list.
--    int32 oneof_index = 7;
          "oneofIndex">:
            doc ("The index of the field type in `Type.oneofs`, for message or enumeration " ++
                 "types. The first type has index 1; zero means the type is not in the list.")
            int32,
--    // Whether to use alternative packed wire representation.
--    bool packed = 8;
          "packed">:
            doc "Whether to use alternative packed wire representation."
            boolean,
--    // The protocol buffer options.
--    repeated Option options = 9;
          "options">:
            doc "The protocol buffer options." $
            list $ pbType "Option",
--    // The field JSON name.
--    string json_name = 10;
          "jsonName">:
            doc "The field JSON name."
            string,
--    // The string value of the default value of this field. Proto2 syntax only.
--    string default_value = 11;
          "defaultValue">:
            doc "The string value of the default value of this field. Proto2 syntax only."
            string],
--  }

--  // Enum type definition.
--  message Enum {
      def "Enum" $
        doc "Enum type definition." $
        record [
--    // Enum type name.
--    string name = 1;
          "name">:
            doc "Enum type name."
            string,
--    // Enum value definitions.
--    repeated EnumValue enumvalue = 2;
          "enumvalue">:
            doc "Enum value definitions." $
            list $ pbType "EnumValue",
--    // Protocol buffer options.
--    repeated Option options = 3;
          "options">:
            doc "Protocol buffer options." $
            list $ pbType "Option",
--    // The source context.
--    SourceContext source_context = 4;
          "sourceContext">:
            doc "The source context." $
            pbSourceContext "SourceContext",
--    // The source syntax.
--    Syntax syntax = 5;
          "syntax">:
            doc "The source syntax." $
            pbType "Syntax"],
--  }

--  // Enum value definition.
--  message EnumValue {
      def "EnumValue" $
        doc "Enum value definition." $
        record [
--    // Enum value name.
--    string name = 1;
          "name">:
            doc "Enum value name."
            string,
--    // Enum value number.
--    int32 number = 2;
          "number">:
            doc "Enum value number."
            int32,
--    // Protocol buffer options.
--    repeated Option options = 3;
          "options">:
            doc "Protocol buffer options." $
            list $ pbType "Option"],
--  }

--  // A protocol buffer option, which can be attached to a message, field,
--  // enumeration, etc.
--  message Option {
      def "Option" $
        doc ("A protocol buffer option, which can be attached to a message, field, " ++
             "enumeration, etc.") $
        record [
--    // The option's name. For protobuf built-in options (options defined in
--    // descriptor.proto), this is the short name. For example, `"map_entry"`.
--    // For custom options, it should be the fully-qualified name. For example,
--    // `"google.api.http"`.
--    string name = 1;
          "name">:
            doc ("The option's name. For protobuf built-in options (options defined in " ++
                 "descriptor.proto), this is the short name. For example, `\"map_entry\"`. " ++
                 "For custom options, it should be the fully-qualified name. For example, " ++
                 "`\"google.api.http\"`.")
            string,
--    // The option's value packed in an Any message. If the value is a primitive,
--    // the corresponding wrapper type defined in google/protobuf/wrappers.proto
--    // should be used. If the value is an enum, it should be stored as an int32
--    // value using the google.protobuf.Int32Value type.
--    Any value = 2;
          "value">:
            doc ("The option's value packed in an Any message. If the value is a primitive, " ++
                 "the corresponding wrapper type defined in google/protobuf/wrappers.proto " ++
                 "should be used. If the value is an enum, it should be stored as an int32 " ++
                 "value using the google.protobuf.Int32Value type.") $
            pbAny "Any"],
--  }

--  // The syntax in which a protocol buffer element is defined.
--  enum Syntax {
      def "Syntax" $
        doc "The syntax in which a protocol buffer element is defined." $
        union [
--    // Syntax `proto2`.
--    SYNTAX_PROTO2 = 0;
          "proto2">: doc "Syntax `proto2`." unit,
--    // Syntax `proto3`.
--    SYNTAX_PROTO3 = 1;
          "proto3">: doc "Syntax `proto3`." unit]]
--  }
