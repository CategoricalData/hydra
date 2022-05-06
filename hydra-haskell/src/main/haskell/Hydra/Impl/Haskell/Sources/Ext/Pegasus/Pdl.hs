{-|
Model for PDL (Pegasus Data Language) schemas

Based on the specification at https://linkedin.github.io/rest.li/pdl_schema
-}

module Hydra.Impl.Haskell.Sources.Ext.Pegasus.Pdl where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Ext.Json.Json

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


pegasusPdlModule = Module pegasusPdl [jsonJsonModule]

pegasusPdlName = "hydra/ext/pegasus/pdl"

pegasusPdl :: Graph Meta
pegasusPdl = Graph pegasusPdlName elements (const True) hydraCoreName
  where
    def = datatype pegasusPdlName
    pdl = nominal . qualify pegasusPdlName
    json = nominal . qualify jsonJsonName

    elements = [

      def "Annotations" $
        doc "Annotations which can be applied to record fields, aliased union members, enum symbols, or named schemas" $
        record [
          field "doc" $ optional string,
          field "deprecated" boolean],

      def "EnumField" $
        record [
          field "name" $ pdl "EnumFieldName",
          field "annotations" $ pdl "Annotations"],

      def "EnumFieldName"
        string,

      def "EnumSchema" $
        record [
          field "fields" $ list $ pdl "EnumField"],

      def "FieldName"
        string,

      def "NamedSchema" $
        record [
          field "qualifiedName" $ pdl "QualifiedName",
          field "type" $ pdl "NamedSchema.Type",
          field "annotations" $ pdl "Annotations"],

      def "NamedSchema.Type" $
        union [
          field "record" $ pdl "RecordSchema",
          field "enum" $ pdl "EnumSchema",
          field "typeref" $ pdl "Schema"],

      def "Name"
        string,

      def "Namespace"
        string,

      def "Package"
        string,

      def "PrimitiveType" $
        enum [
          "boolean",
          "bytes",
          "double",
          "float",
          "int",
          "long",
          "string"],

      def "PropertyKey"
        string,

      def "Property" $
        record [
          field "key" $ pdl "PropertyKey",
          field "value" $ optional $ json "Value"],

      def "QualifiedName" $
        record [
          field "name" $ pdl "Name",
          field "namespace" $ optional $ pdl "Namespace"],

      def "RecordField" $
        record [
          field "name" $ pdl "FieldName",
          field "value" $ pdl "Schema",
          field "optional" boolean,
          -- Note: the default value for an enum-valued field must be one of the enumerated string symbols
          field "default" $ optional $ json "Value",
          field "annotations" $ pdl "Annotations"],

      def "RecordSchema" $
        record [
          field "fields" $ list $ pdl "RecordField",
          -- Note: all included schemas must be record schemas
          field "includes" $ list $ pdl "NamedSchema"],

      def "Schema" $
        union [
          field "array" $ pdl "Schema",
          field "fixed" int32,
          field "inline" $ pdl "NamedSchema",
          field "map" $ pdl "Schema",
          field "named" $ pdl "QualifiedName",
          field "null" unit,
          field "primitive" $ pdl "PrimitiveType",
          field "union" $ pdl "UnionSchema"],

      def "SchemaFile" $
        record [
          field "namespace" $ pdl "Namespace",
          field "package" $ optional $ pdl "Package",
          field "imports" $ list $ pdl "QualifiedName",
          field "schemas" $ list $ pdl "NamedSchema"],

      def "UnionMember" $
        record [
          field "alias" $ optional $ pdl "FieldName",
          field "value" $ pdl "Schema",
          -- Note: annotations are only available for aliased members
          field "annotations" $ pdl "Annotations"],

      -- Note: unions are not allowed as member types of other unions
      def "UnionSchema" $
        list $ pdl "UnionMember"]
