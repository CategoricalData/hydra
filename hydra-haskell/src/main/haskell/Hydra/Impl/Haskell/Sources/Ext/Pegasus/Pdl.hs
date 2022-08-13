{-|
Model for PDL (Pegasus Data Language) schemas

Based on the specification at https://linkedin.github.io/rest.li/pdl_schema
-}

module Hydra.Impl.Haskell.Sources.Ext.Pegasus.Pdl where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Ext.Json.Model

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


pegasusPdlModule :: Module Meta
pegasusPdlModule = Module pegasusPdl [jsonModelModule]

pegasusPdlName :: GraphName
pegasusPdlName = GraphName "hydra/ext/pegasus/pdl"

pegasusPdl :: Graph Meta
pegasusPdl = Graph pegasusPdlName elements hydraCoreName
  where
    def = datatype coreContext pegasusPdlName
    pdl = nsref pegasusPdlName
    json = nsref jsonModelName

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

      def "EnumFieldName"
        string,

      def "EnumSchema" $
        record [
          "fields">: list $ pdl "EnumField"],

      def "FieldName"
        string,

      def "NamedSchema" $
        record [
          "qualifiedName">: pdl "QualifiedName",
          "type">: pdl "NamedSchema.Type",
          "annotations">: pdl "Annotations"],

      def "NamedSchema.Type" $
        union [
          "record">: pdl "RecordSchema",
          "enum">: pdl "EnumSchema",
          "typeref">: pdl "Schema"],

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
        list $ pdl "UnionMember"]
