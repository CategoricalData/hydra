{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Parquet.Delta where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


deltaParquetModule :: Module Kv
deltaParquetModule = Module ns elements [hydraCoreModule] tier0Modules $
    Just ("A partial Delta Parquet model")
  where
    ns = Namespace "hydra/langs/parquet/delta"
    def = datatype ns
    delta = typeref ns

    elements = [
      def "ArrayType" $
        record [
          "elementType">: delta "DataType",
          "containsNull">: boolean],

      def "DataType" $
        union [
          "array">: delta "ArrayType",
          "binary">: unit,
          "boolean">: unit,
          "byte">: unit,
          "date">: unit,
          "decimal">: delta "DecimalType",
          "double">: unit,
          "float">: unit,
          "integer">: unit,
          "long">: unit,
          "map">: delta "MapType",
          "null">: unit,
          "short">: unit,
          "string">: unit,
          "struct">: delta "StructType"],

      def "DecimalType" $
        record [
          "precision">: int32,
          "scale">: int32],

      def "MapType" $
        record [
          "keyType">: delta "DataType",
          "valueType">: delta "DataType",
          "valueContainsNull">: boolean],

      def "StructField" $
        record [
          "name">: string,
          "dataType">: delta "DataType",
          "nullable">: boolean],

      def "StructType" $
        record [
          "fields">: list $ delta "StructField"]]
