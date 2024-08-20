{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Ext.Delta.Parquet where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


deltaParquetModule :: Module
deltaParquetModule = Module ns elements [hydraCoreModule] tier0Modules $
    Just ("A partial Delta Parquet model, based on DataType and its subclasses as specified in the 3.0.0 Java API:"
      ++ " https://docs.delta.io/3.0.0/api/java/kernel/io/delta/kernel/types/DataType.html")
  where
    ns = Namespace "hydra/ext/delta/parquet"
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
