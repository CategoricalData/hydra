{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Avro.Schema where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Classes     as Classes
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Variants    as Variants
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


avroSchemaModule :: Module
avroSchemaModule = Module ns elements [Json.module_] [Core.module_] $
    Just ("A model for Avro schemas. Based on the Avro 1.11.1 specification:\n" ++
      "  https://avro.apache.org/docs/1.11.1/specification")
  where
    ns = Namespace "hydra.ext.org.apache.avro.schema"
    def = datatype ns
    avro = typeref ns
    json = typeref $ moduleNamespace Json.module_

    elements = [
      def "Array" $
        record [
          "items">: avro "Schema"],

      def "Enum" $
        record [
          "symbols">:
            doc ("a JSON array, listing symbols, as JSON strings. All symbols in an enum must be unique; " ++
              "duplicates are prohibited. Every symbol must match the regular expression [A-Za-z_][A-Za-z0-9_]* " ++
              "(the same requirement as for names)") $
            list string,
          "default">:
            doc ("A default value for this enumeration, used during resolution when the reader encounters " ++
              "a symbol from the writer that isn't defined in the reader's schema. " ++
              "The value provided here must be a JSON string that's a member of the symbols array") $
            optional string],

      def "Field" $
        record [
          "name">:
            doc "a JSON string providing the name of the field"
            string,
          "doc">:
            doc "a JSON string describing this field for users" $
            optional string,
          "type">:
            doc "a schema" $
            avro "Schema",
          "default">:
            doc "default value for this field, only used when reading instances that lack the field for schema evolution purposes" $
            optional $ json "Value",
          "order">:
            doc "specifies how this field impacts sort ordering of this record" $
            optional $ avro "Order",
          "aliases">:
            doc "a JSON array of strings, providing alternate names for this field" $
            optional $ list string,
          "annotations">:
            doc "Any additional key/value pairs attached to the field" $
            Types.map string $ json "Value"],

      def "Fixed" $
        record [
          "size">:
            doc "an integer, specifying the number of bytes per value"
            int32],

      def "Map" $
        record [
          "values">: avro "Schema"],

      def "Named" $
        record [
          "name">:
            doc "a string naming this schema"
            string,
          "namespace">:
            doc "a string that qualifies the name" $
            optional string,
          "aliases">:
            doc "a JSON array of strings, providing alternate names for this schema" $
            optional $ list string,
          "doc">:
            doc "a JSON string providing documentation to the user of this schema" $
            optional string,
          "type">: avro "NamedType",
          "annotations">:
            doc "Any additional key/value pairs attached to the type" $
            Types.map string $ json "Value"],

      def "NamedType" $
        union [
          "enum">: avro "Enum",
          "fixed">: avro "Fixed",
          "record">: avro "Record"],

      def "Order" $
        enum ["ascending", "descending", "ignore"],

      def "Primitive" $
        union [
          "null">:
            doc "no value" unit,
          "boolean">:
            doc "A binary value" unit,
          "int">:
            doc "32-bit signed integer" unit,
          "long">:
            doc "64-bit signed integer" unit,
          "float">:
            doc "single precision (32-bit) IEEE 754 floating-point number" unit,
          "double">:
            doc "double precision (64-bit) IEEE 754 floating-point number" unit,
          "bytes">:
            doc "sequence of 8-bit unsigned bytes" unit,
          "string">:
            doc "unicode character sequence" unit],

      def "Record" $
        record [
          "fields">:
            doc "a JSON array, listing fields" $
            list $ avro "Field"],

      def "Schema" $
        union [
          "array">: avro "Array",
          "map">: avro "Map",
          "named">: avro "Named",
          "primitive">: avro "Primitive",
          "reference">: -- Note: "reference" is not described in the Avro specification; this has been added
            doc "A reference by name to a previously defined type" string,
          "union">: avro "Union"
        ],

      def "Union" $
        wrap $ list $ avro "Schema"]
