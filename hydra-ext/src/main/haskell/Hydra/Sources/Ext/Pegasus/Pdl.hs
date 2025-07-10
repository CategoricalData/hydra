module Hydra.Sources.Ext.Pegasus.Pdl where

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


pegasusPdlModule :: Module
pegasusPdlModule = Module ns elements [Json.module_] [Core.module_] $
    Just ("A model for PDL (Pegasus Data Language) schemas. Based on the specification at:\n" ++
      "  https://linkedin.github.io/rest.li/pdl_schema")
  where
    ns = Namespace "hydra.ext.pegasus.pdl"
    def = datatype ns
    pdl = typeref ns
    json = typeref $ moduleNamespace Json.module_

    elements = [

      def "Annotations" $
        doc "Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas" $
        record [
          "doc">: optional string,
          "deprecated">: boolean],

      def "EnumField" $
        record [
          "name">: pdl "EnumFieldName",
          "annotations">: pdl "Annotations"],

      def "EnumFieldName" $
        wrap string,

      def "EnumSchema" $
        record [
          "fields">: list $ pdl "EnumField"],

      def "FieldName" $
        wrap string,

      def "NamedSchema" $
        record [
          "qualifiedName">: pdl "QualifiedName",
          "type">: pdl "NamedSchemaType",
          "annotations">: pdl "Annotations"],

      def "NamedSchemaType" $
        union [
          "record">: pdl "RecordSchema",
          "enum">: pdl "EnumSchema",
          "typeref">: pdl "Schema"],

      def "Name" $
        wrap string,

      def "Namespace" $
        wrap string,

      def "Package" $
        wrap string,

      def "PrimitiveType" $
        enum [
          "boolean",
          "bytes",
          "double",
          "float",
          "int",
          "long",
          "string"],

      def "PropertyKey" $
        wrap string,

      def "Property" $
        record [
          "key">: pdl "PropertyKey",
          "value">: optional $ json "Value"],

      def "QualifiedName" $
        record [
          "name">: pdl "Name",
          "namespace">: optional $ pdl "Namespace"],

      def "RecordField" $
        record [
          "name">: pdl "FieldName",
          "value">: pdl "Schema",
          "optional">: boolean,
          -- Note: the default value for an enum-valued must be one of the enumerated string symbols
          "default">: optional $ json "Value",
          "annotations">: pdl "Annotations"],

      def "RecordSchema" $
        record [
          "fields">: list $ pdl "RecordField",
          -- Note: all included schemas must be record schemas
          "includes">: list $ pdl "NamedSchema"],

      def "Schema" $
        union [
          "array">: pdl "Schema",
          "fixed">: int32,
          "inline">: pdl "NamedSchema",
          "map">: pdl "Schema",
          "named">: pdl "QualifiedName",
          "null">: unit,
          "primitive">: pdl "PrimitiveType",
          "union">: pdl "UnionSchema"],

      def "SchemaFile" $
        record [
          "namespace">: pdl "Namespace",
          "package">: optional $ pdl "Package",
          "imports">: list $ pdl "QualifiedName",
          "schemas">: list $ pdl "NamedSchema"],

      def "UnionMember" $
        record [
          "alias">: optional $ pdl "FieldName",
          "value">: pdl "Schema",
          -- Note: annotations are only available for aliased members
          "annotations">: pdl "Annotations"],

      -- Note: unions are not allowed as member types of other unions
      def "UnionSchema" $
        wrap $ list $ pdl "UnionMember"]
