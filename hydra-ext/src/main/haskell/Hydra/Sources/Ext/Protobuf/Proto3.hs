{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.Protobuf.Proto3 where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Mantle      as Mantle
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow


proto3Ns = Namespace "hydra.ext.protobuf.proto3"
proto3 = typeref proto3Ns

proto3Module :: Module
proto3Module = Module proto3Ns elements [Core.module_] [Core.module_] $
    Just ("A model for Protocol Buffers v3 enum and message types, designed as a target for transformations."
      ++ "This model is loosely based on https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/type.proto,"
      ++ " as well as the proto3 reference documentation")
  where
    def = datatype proto3Ns
    elements = [

      def "Definition" $
        union [
          "enum">: proto3 "EnumDefinition",
          "message">: proto3 "MessageDefinition"],

      def "EnumDefinition" $
        doc "Enum type definition" $
        record [
          "name">:
            doc "Enum type name" $
            proto3 "TypeName",
          "values">:
            doc "Enum value definitions" $
            list $ proto3 "EnumValue",
          "options">:
            doc "Protocol buffer options" $
            list $ proto3 "Option"],

      def "EnumValue" $
        doc "Enum value definition" $
        record [
          "name">:
            doc "Enum value name" $
            proto3 "EnumValueName",
          "number">:
            doc "Enum value number"
            int32,
          "options">:
            doc "Protocol buffer options" $
            list $ proto3 "Option"],

      def "EnumValueName" $
        wrap string,

      def "Field" $
        doc "A single field of a message type" $
        record [
          "name">:
            doc "The field name" $
            proto3 "FieldName",
          "jsonName">:
            doc "The field JSON name" $
            optional string,
          "type">:
            doc "The datatype of the field" $
            proto3 "FieldType",
          "number">:
            doc "The field number"
            int32,
          "options">:
            doc "The protocol buffer options" $
            list $ proto3 "Option"],

      def "FieldName" $
        doc "The name of a field" $
        wrap string,

      def "FieldType" $
        union [
          "map">: proto3 "MapType",
          "oneof">: list $ proto3 "Field",
          "repeated">: proto3 "SimpleType",
          "simple">: proto3 "SimpleType"],

      def "FileReference" $
        wrap string,

      def "MapType" $
        record [
          "keys">: proto3 "SimpleType",
          "values">: proto3 "SimpleType"],

      def "MessageDefinition" $
        doc "A protocol buffer message type" $
        record [
          "name">:
            doc "The fully qualified message name" $
            proto3 "TypeName",
          "fields">:
            doc "The list of fields" $
            list $ proto3 "Field",
          "options">:
            doc "The protocol buffer options" $
            list $ proto3 "Option"],

      def "Option" $
        doc ("A protocol buffer option, which can be attached to a message, field, " ++
             "enumeration, etc") $
        record [
          "name">:
            doc ("The option's name. For protobuf built-in options (options defined in " ++
                 "descriptor.proto), this is the short name. For example, `\"map_entry\"`. " ++
                 "For custom options, it should be the fully-qualified name. For example, " ++
                 "`\"google.api.http\"`.")
            string,
          "value">:
            doc ("The option's value") $
            proto3 "Value"],

      def "PackageName" $
        wrap string,

      def "ProtoFile" $
        doc "A .proto file, usually containing one or more enum or message type definitions" $
        record [
          "package">: proto3 "PackageName",
          "imports">: list $ proto3 "FileReference",
          "types">: list $ proto3 "Definition",
          "options">: list $ proto3 "Option"],

      def "ScalarType" $
        doc "One of several Proto3 scalar types" $
        enum [
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
          "uint64"],

      def "SimpleType" $
        doc "A scalar type or a reference to an enum type or message type" $
        union [
          "reference">: proto3 "TypeName",
          "scalar">: proto3 "ScalarType"],

      def "TypeName" $
        doc "The local name of an enum type or message type" $
        wrap string,

      def "TypeReference" $
        doc "A reference to an enum type or message type" $
        wrap string,

      def "Value" $
        doc "A scalar value" $
        union [
          "boolean">: boolean,
          "string">: string
          -- Add other scalar value types as needed
        ]]
