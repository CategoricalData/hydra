{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Delta.Parquet where

-- Standard imports for type-level sources outside of the kernel
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
import qualified Hydra.Sources.Kernel.Types.Meta        as Meta
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


deltaParquetModule :: Module
deltaParquetModule = Module ns elements [Core.module_] [Core.module_] $
    Just ("A partial Delta Parquet model, based on DataType and its subclasses as specified in the 3.0.0 Java API:"
      ++ " https://docs.delta.io/3.0.0/api/java/kernel/io/delta/kernel/types/DataType.html")
  where
    ns = Namespace "hydra.ext.io.delta.parquet"
    def = datatype ns
    enumVal name desc = name>: doc desc unit
    delta = typeref ns

    elements = [
      def "ArrayType" $
        doc "Represent array data type." $
        record [
          "elementType">: delta "DataType",
          "containsNull">: boolean],

      def "BasePrimitiveType" $
        doc "Base class for all primitive types DataType." $
        union [
          enumVal "binary" "The data type representing byte[] values.",
          enumVal "boolean" "Data type representing boolean type values.",
          enumVal "byte" "The data type representing byte type values.",
          enumVal "date" (
            "A date type, supporting \"0001-01-01\" through \"9999-12-31\"."
            ++ " Internally, this is represented as the number of days from 1970-01-01."),
          enumVal "double" "The data type representing double type values.",
          enumVal "float" "The data type representing float type values.",
          enumVal "integer" "The data type representing integer type values.",
          enumVal "long" "The data type representing long type values.",
          enumVal "short" "The data type representing short type values.",
          enumVal "string" "The data type representing string type values.",
          enumVal "timestamp" (
            "A timestamp type, supporting [0001-01-01T00:00:00.000000Z, 9999-12-31T23:59:59.999999Z]"
            ++ " where the left/right-bound is a date and time of the proleptic Gregorian calendar in UTC+00:00."
            ++ " Internally, this is represented as the number of microseconds since the Unix epoch,"
            ++ " 1970-01-01 00:00:00 UTC.")],

      def "DataType" $
        union [
          "array">:
            doc "Represent array data type." $
            delta "ArrayType",
          "base">:
            doc "Base class for all primitive types DataType." $
            delta "BasePrimitiveType",
          "decimal">:
            doc "A decimal data type." $
            delta "DecimalType",
          "map">:
            doc "Data type representing a map type." $
            delta "MapType",
          "struct">:
            doc "Struct type which contains one or more columns." $
            delta "StructType"],

      def "DecimalType" $
        doc ("A decimal data type with fixed precision (the maximum number of digits)"
          ++ " and scale (the number of digits on right side of dot)."
          ++ " The precision can be up to 38, scale can also be up to 38 (less or equal to precision).") $
        record [
          "precision">: int32,
          "scale">: int32],

      def "MapType" $
        doc "Data type representing a map type." $
        record [
          "keyType">: delta "DataType",
          "valueType">: delta "DataType",
          "valueContainsNull">: boolean],

      def "StructField" $
        doc "Represents a subfield of StructType with additional properties and metadata." $
        record [
          "name">: string,
          "dataType">: delta "DataType",
          "nullable">: boolean],

      def "StructType" $
        doc "Struct type which contains one or more columns." $
        record [
          "fields">: list $ delta "StructField"]]
