{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Parquet.Delta where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


deltaParquetModule :: Module Kv
deltaParquetModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
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
          "map">: delta "MapType",
          "scalar">: delta "ScalarType",
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

      def "NumberType" $
        union [
          "byte">: unit,
          "decimal">: delta "DecimalType",
          "double">: unit,
          "float">: unit,
          "integer">: unit,
          "long">: unit,
          "short">: unit],

      def "ScalarType" $
        union [
          "binary">: unit,
          "boolean">: unit,
          "date">: unit,
          "null">: unit,
          "number">: delta "NumberType",
          "string">: unit],

      def "StructField" $
        record [
          "name">: string,
          "dataType">: delta "DataType",
          "nullable">: boolean],

      def "StructType" $
        record [
          "fields">: list $ delta "StructField"]]
