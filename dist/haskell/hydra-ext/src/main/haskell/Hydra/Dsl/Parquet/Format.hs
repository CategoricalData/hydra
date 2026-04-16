-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.parquet.format

module Hydra.Dsl.Parquet.Format where

import qualified Hydra.Core as Core
import qualified Hydra.Parquet.Format as Format
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Int as I

aesGcmCtrV1 :: Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.AesGcmCtrV1
aesGcmCtrV1 aadPrefix aadFileUnique supplyAadPrefix =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm aadPrefix)},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Phantoms.unTTerm aadFileUnique)},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm supplyAadPrefix)}]}))

aesGcmCtrV1AadFileUnique :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm (Maybe B.ByteString)
aesGcmCtrV1AadFileUnique x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
        Core.projectionField = (Core.Name "aadFileUnique")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aesGcmCtrV1AadPrefix :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm (Maybe B.ByteString)
aesGcmCtrV1AadPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
        Core.projectionField = (Core.Name "aadPrefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aesGcmCtrV1SupplyAadPrefix :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm (Maybe Bool)
aesGcmCtrV1SupplyAadPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
        Core.projectionField = (Core.Name "supplyAadPrefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aesGcmCtrV1WithAadFileUnique :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.AesGcmCtrV1
aesGcmCtrV1WithAadFileUnique original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
              Core.projectionField = (Core.Name "aadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
              Core.projectionField = (Core.Name "supplyAadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aesGcmCtrV1WithAadPrefix :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.AesGcmCtrV1
aesGcmCtrV1WithAadPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
              Core.projectionField = (Core.Name "aadFileUnique")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
              Core.projectionField = (Core.Name "supplyAadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aesGcmCtrV1WithSupplyAadPrefix :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.AesGcmCtrV1
aesGcmCtrV1WithSupplyAadPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
              Core.projectionField = (Core.Name "aadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmCtrV1"),
              Core.projectionField = (Core.Name "aadFileUnique")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

aesGcmV1 :: Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.AesGcmV1
aesGcmV1 aadPrefix aadFileUnique supplyAadPrefix =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm aadPrefix)},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Phantoms.unTTerm aadFileUnique)},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm supplyAadPrefix)}]}))

aesGcmV1AadFileUnique :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm (Maybe B.ByteString)
aesGcmV1AadFileUnique x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
        Core.projectionField = (Core.Name "aadFileUnique")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aesGcmV1AadPrefix :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm (Maybe B.ByteString)
aesGcmV1AadPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
        Core.projectionField = (Core.Name "aadPrefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aesGcmV1SupplyAadPrefix :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm (Maybe Bool)
aesGcmV1SupplyAadPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
        Core.projectionField = (Core.Name "supplyAadPrefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

aesGcmV1WithAadFileUnique :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.AesGcmV1
aesGcmV1WithAadFileUnique original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
              Core.projectionField = (Core.Name "aadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
              Core.projectionField = (Core.Name "supplyAadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aesGcmV1WithAadPrefix :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.AesGcmV1
aesGcmV1WithAadPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
              Core.projectionField = (Core.Name "aadFileUnique")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
              Core.projectionField = (Core.Name "supplyAadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

aesGcmV1WithSupplyAadPrefix :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.AesGcmV1
aesGcmV1WithSupplyAadPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "aadPrefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
              Core.projectionField = (Core.Name "aadPrefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aadFileUnique"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.AesGcmV1"),
              Core.projectionField = (Core.Name "aadFileUnique")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supplyAadPrefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bloomFilterAlgorithmBlock :: Phantoms.TTerm Format.BloomFilterAlgorithm
bloomFilterAlgorithmBlock =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterAlgorithm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = Core.TermUnit}}))

bloomFilterCompressionUncompressed :: Phantoms.TTerm Format.BloomFilterCompression
bloomFilterCompressionUncompressed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterCompression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uncompressed"),
        Core.fieldTerm = Core.TermUnit}}))

bloomFilterHashXxhash :: Phantoms.TTerm Format.BloomFilterHash
bloomFilterHashXxhash =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHash"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xxhash"),
        Core.fieldTerm = Core.TermUnit}}))

bloomFilterHeader :: Phantoms.TTerm Int -> Phantoms.TTerm Format.BloomFilterAlgorithm -> Phantoms.TTerm Format.BloomFilterHash -> Phantoms.TTerm Format.BloomFilterCompression -> Phantoms.TTerm Format.BloomFilterHeader
bloomFilterHeader numBytes algorithm hash compression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numBytes"),
          Core.fieldTerm = (Phantoms.unTTerm numBytes)},
        Core.Field {
          Core.fieldName = (Core.Name "algorithm"),
          Core.fieldTerm = (Phantoms.unTTerm algorithm)},
        Core.Field {
          Core.fieldName = (Core.Name "hash"),
          Core.fieldTerm = (Phantoms.unTTerm hash)},
        Core.Field {
          Core.fieldName = (Core.Name "compression"),
          Core.fieldTerm = (Phantoms.unTTerm compression)}]}))

bloomFilterHeaderAlgorithm :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Format.BloomFilterAlgorithm
bloomFilterHeaderAlgorithm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
        Core.projectionField = (Core.Name "algorithm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bloomFilterHeaderCompression :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Format.BloomFilterCompression
bloomFilterHeaderCompression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
        Core.projectionField = (Core.Name "compression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bloomFilterHeaderHash :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Format.BloomFilterHash
bloomFilterHeaderHash x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
        Core.projectionField = (Core.Name "hash")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bloomFilterHeaderNumBytes :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Int
bloomFilterHeaderNumBytes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
        Core.projectionField = (Core.Name "numBytes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bloomFilterHeaderWithAlgorithm :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Format.BloomFilterAlgorithm -> Phantoms.TTerm Format.BloomFilterHeader
bloomFilterHeaderWithAlgorithm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numBytes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "numBytes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "algorithm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "hash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "hash")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "compression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bloomFilterHeaderWithCompression :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Format.BloomFilterCompression -> Phantoms.TTerm Format.BloomFilterHeader
bloomFilterHeaderWithCompression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numBytes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "numBytes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "algorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "algorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "hash")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bloomFilterHeaderWithHash :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Format.BloomFilterHash -> Phantoms.TTerm Format.BloomFilterHeader
bloomFilterHeaderWithHash original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numBytes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "numBytes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "algorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "algorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hash"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "compression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "compression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bloomFilterHeaderWithNumBytes :: Phantoms.TTerm Format.BloomFilterHeader -> Phantoms.TTerm Int -> Phantoms.TTerm Format.BloomFilterHeader
bloomFilterHeaderWithNumBytes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numBytes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "algorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "algorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "hash"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "hash")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.BloomFilterHeader"),
              Core.projectionField = (Core.Name "compression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

boundaryOrderAscending :: Phantoms.TTerm Format.BoundaryOrder
boundaryOrderAscending =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.BoundaryOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascending"),
        Core.fieldTerm = Core.TermUnit}}))

boundaryOrderDescending :: Phantoms.TTerm Format.BoundaryOrder
boundaryOrderDescending =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.BoundaryOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "descending"),
        Core.fieldTerm = Core.TermUnit}}))

boundaryOrderUnordered :: Phantoms.TTerm Format.BoundaryOrder
boundaryOrderUnordered =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.BoundaryOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unordered"),
        Core.fieldTerm = Core.TermUnit}}))

columnChunk :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm (Maybe Format.ColumnMetaData) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm (Maybe Format.ColumnCryptoMetaData) -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.ColumnChunk
columnChunk filePath fileOffset metaData offsetIndexOffset offsetIndexLength columnIndexOffset columnIndexLength cryptoMetadata encryptedColumnMetadata =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Phantoms.unTTerm filePath)},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Phantoms.unTTerm fileOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Phantoms.unTTerm metaData)},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Phantoms.unTTerm offsetIndexOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Phantoms.unTTerm offsetIndexLength)},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Phantoms.unTTerm columnIndexOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Phantoms.unTTerm columnIndexLength)},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm cryptoMetadata)},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm encryptedColumnMetadata)}]}))

columnChunkColumnIndexLength :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Int)
columnChunkColumnIndexLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "columnIndexLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkColumnIndexOffset :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe I.Int64)
columnChunkColumnIndexOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "columnIndexOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkCryptoMetadata :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Format.ColumnCryptoMetaData)
columnChunkCryptoMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "cryptoMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkEncryptedColumnMetadata :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe B.ByteString)
columnChunkEncryptedColumnMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkFileOffset :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm I.Int64
columnChunkFileOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "fileOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkFilePath :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe String)
columnChunkFilePath x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "filePath")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkMetaData :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Format.ColumnMetaData)
columnChunkMetaData x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "metaData")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkOffsetIndexLength :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Int)
columnChunkOffsetIndexLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "offsetIndexLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkOffsetIndexOffset :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe I.Int64)
columnChunkOffsetIndexOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
        Core.projectionField = (Core.Name "offsetIndexOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnChunkWithColumnIndexLength :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithColumnIndexLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithColumnIndexOffset :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithColumnIndexOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithCryptoMetadata :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Format.ColumnCryptoMetaData) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithCryptoMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithEncryptedColumnMetadata :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithEncryptedColumnMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnChunkWithFileOffset :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithFileOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithFilePath :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithFilePath original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithMetaData :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Format.ColumnMetaData) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithMetaData original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithOffsetIndexLength :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithOffsetIndexLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnChunkWithOffsetIndexOffset :: Phantoms.TTerm Format.ColumnChunk -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.ColumnChunk
columnChunkWithOffsetIndexOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "filePath"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "filePath")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metaData"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "metaData")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "offsetIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "offsetIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnIndexLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "columnIndexLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cryptoMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "cryptoMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptedColumnMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnChunk"),
              Core.projectionField = (Core.Name "encryptedColumnMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnCryptoMetaDataEncryptionWithColumnKey :: Phantoms.TTerm Format.EncryptionWithColumnKey -> Phantoms.TTerm Format.ColumnCryptoMetaData
columnCryptoMetaDataEncryptionWithColumnKey x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.ColumnCryptoMetaData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "encryptionWithColumnKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnCryptoMetaDataEncryptionWithFooterKey :: Phantoms.TTerm Format.EncryptionWithFooterKey -> Phantoms.TTerm Format.ColumnCryptoMetaData
columnCryptoMetaDataEncryptionWithFooterKey x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.ColumnCryptoMetaData"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "encryptionWithFooterKey"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

columnIndex :: Phantoms.TTerm [Bool] -> Phantoms.TTerm [B.ByteString] -> Phantoms.TTerm [B.ByteString] -> Phantoms.TTerm Format.BoundaryOrder -> Phantoms.TTerm (Maybe [I.Int64]) -> Phantoms.TTerm Format.ColumnIndex
columnIndex nullPages minValues maxValues boundaryOrder nullCounts =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullPages"),
          Core.fieldTerm = (Phantoms.unTTerm nullPages)},
        Core.Field {
          Core.fieldName = (Core.Name "minValues"),
          Core.fieldTerm = (Phantoms.unTTerm minValues)},
        Core.Field {
          Core.fieldName = (Core.Name "maxValues"),
          Core.fieldTerm = (Phantoms.unTTerm maxValues)},
        Core.Field {
          Core.fieldName = (Core.Name "boundaryOrder"),
          Core.fieldTerm = (Phantoms.unTTerm boundaryOrder)},
        Core.Field {
          Core.fieldName = (Core.Name "nullCounts"),
          Core.fieldTerm = (Phantoms.unTTerm nullCounts)}]}))

columnIndexBoundaryOrder :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm Format.BoundaryOrder
columnIndexBoundaryOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
        Core.projectionField = (Core.Name "boundaryOrder")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnIndexMaxValues :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm [B.ByteString]
columnIndexMaxValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
        Core.projectionField = (Core.Name "maxValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnIndexMinValues :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm [B.ByteString]
columnIndexMinValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
        Core.projectionField = (Core.Name "minValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnIndexNullCounts :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm (Maybe [I.Int64])
columnIndexNullCounts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
        Core.projectionField = (Core.Name "nullCounts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnIndexNullPages :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm [Bool]
columnIndexNullPages x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
        Core.projectionField = (Core.Name "nullPages")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnIndexWithBoundaryOrder :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm Format.BoundaryOrder -> Phantoms.TTerm Format.ColumnIndex
columnIndexWithBoundaryOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullPages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullPages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "minValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "maxValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundaryOrder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "nullCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnIndexWithMaxValues :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm [B.ByteString] -> Phantoms.TTerm Format.ColumnIndex
columnIndexWithMaxValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullPages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullPages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "minValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "boundaryOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "boundaryOrder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnIndexWithMinValues :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm [B.ByteString] -> Phantoms.TTerm Format.ColumnIndex
columnIndexWithMinValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullPages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullPages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "maxValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "maxValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundaryOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "boundaryOrder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnIndexWithNullCounts :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm (Maybe [I.Int64]) -> Phantoms.TTerm Format.ColumnIndex
columnIndexWithNullCounts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullPages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullPages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "minValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "maxValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundaryOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "boundaryOrder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullCounts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnIndexWithNullPages :: Phantoms.TTerm Format.ColumnIndex -> Phantoms.TTerm [Bool] -> Phantoms.TTerm Format.ColumnIndex
columnIndexWithNullPages original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullPages"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "minValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "minValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "maxValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundaryOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "boundaryOrder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnIndex"),
              Core.projectionField = (Core.Name "nullCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaData :: Phantoms.TTerm Format.Type -> Phantoms.TTerm [Format.Encoding] -> Phantoms.TTerm [String] -> Phantoms.TTerm Format.CompressionCodec -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm (Maybe [Format.KeyValue]) -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe Format.Statistics) -> Phantoms.TTerm (Maybe [Format.PageEncodingStats]) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaData type_ encodings pathInSchema codec numValues totalUncompressedSize totalCompressedSize keyValueMetadata dataPageOffset indexPageOffset dictionaryPageOffset statistics encodingStats bloomFilterOffset =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Phantoms.unTTerm encodings)},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Phantoms.unTTerm pathInSchema)},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Phantoms.unTTerm codec)},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm numValues)},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Phantoms.unTTerm totalUncompressedSize)},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Phantoms.unTTerm totalCompressedSize)},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm keyValueMetadata)},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Phantoms.unTTerm dataPageOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Phantoms.unTTerm indexPageOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Phantoms.unTTerm dictionaryPageOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Phantoms.unTTerm statistics)},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Phantoms.unTTerm encodingStats)},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Phantoms.unTTerm bloomFilterOffset)}]}))

columnMetaDataBloomFilterOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe I.Int64)
columnMetaDataBloomFilterOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "bloomFilterOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataCodec :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm Format.CompressionCodec
columnMetaDataCodec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "codec")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataDataPageOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64
columnMetaDataDataPageOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "dataPageOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataDictionaryPageOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe I.Int64)
columnMetaDataDictionaryPageOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "dictionaryPageOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataEncodingStats :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe [Format.PageEncodingStats])
columnMetaDataEncodingStats x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "encodingStats")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataEncodings :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm [Format.Encoding]
columnMetaDataEncodings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "encodings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataIndexPageOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe I.Int64)
columnMetaDataIndexPageOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "indexPageOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataKeyValueMetadata :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe [Format.KeyValue])
columnMetaDataKeyValueMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "keyValueMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataNumValues :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64
columnMetaDataNumValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "numValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataPathInSchema :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm [String]
columnMetaDataPathInSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "pathInSchema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataStatistics :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe Format.Statistics)
columnMetaDataStatistics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "statistics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataTotalCompressedSize :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64
columnMetaDataTotalCompressedSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "totalCompressedSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataTotalUncompressedSize :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64
columnMetaDataTotalUncompressedSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "totalUncompressedSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataType :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm Format.Type
columnMetaDataType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnMetaDataWithBloomFilterOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithBloomFilterOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnMetaDataWithCodec :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm Format.CompressionCodec -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithCodec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithDataPageOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithDataPageOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithDictionaryPageOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithDictionaryPageOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithEncodingStats :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe [Format.PageEncodingStats]) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithEncodingStats original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithEncodings :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm [Format.Encoding] -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithEncodings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithIndexPageOffset :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithIndexPageOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithKeyValueMetadata :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe [Format.KeyValue]) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithKeyValueMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithNumValues :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithNumValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithPathInSchema :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm [String] -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithPathInSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithStatistics :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm (Maybe Format.Statistics) -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithStatistics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithTotalCompressedSize :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithTotalCompressedSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithTotalUncompressedSize :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithTotalUncompressedSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnMetaDataWithType :: Phantoms.TTerm Format.ColumnMetaData -> Phantoms.TTerm Format.Type -> Phantoms.TTerm Format.ColumnMetaData
columnMetaDataWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "codec")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalUncompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalUncompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dataPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "indexPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "dictionaryPageOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodingStats"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "encodingStats")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bloomFilterOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.ColumnMetaData"),
              Core.projectionField = (Core.Name "bloomFilterOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnOrderTypeOrder :: Phantoms.TTerm Format.ColumnOrder
columnOrderTypeOrder =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.ColumnOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeOrder"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecBrotli :: Phantoms.TTerm Format.CompressionCodec
compressionCodecBrotli =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brotli"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecGzip :: Phantoms.TTerm Format.CompressionCodec
compressionCodecGzip =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gzip"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecLz4Raw :: Phantoms.TTerm Format.CompressionCodec
compressionCodecLz4Raw =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lz4Raw"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecLzo :: Phantoms.TTerm Format.CompressionCodec
compressionCodecLzo =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lzo"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecSnappy :: Phantoms.TTerm Format.CompressionCodec
compressionCodecSnappy =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "snappy"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecUncompressed :: Phantoms.TTerm Format.CompressionCodec
compressionCodecUncompressed =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uncompressed"),
        Core.fieldTerm = Core.TermUnit}}))

compressionCodecZstd :: Phantoms.TTerm Format.CompressionCodec
compressionCodecZstd =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.CompressionCodec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "zstd"),
        Core.fieldTerm = Core.TermUnit}}))

dataPageHeader :: Phantoms.TTerm Int -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm (Maybe Format.Statistics) -> Phantoms.TTerm Format.DataPageHeader
dataPageHeader numValues encoding definitionLevelEncoding repetitionLevelEncoding statistics =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm numValues)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm encoding)},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelEncoding"),
          Core.fieldTerm = (Phantoms.unTTerm definitionLevelEncoding)},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelEncoding"),
          Core.fieldTerm = (Phantoms.unTTerm repetitionLevelEncoding)},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Phantoms.unTTerm statistics)}]}))

dataPageHeaderDefinitionLevelEncoding :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Format.Encoding
dataPageHeaderDefinitionLevelEncoding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
        Core.projectionField = (Core.Name "definitionLevelEncoding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderEncoding :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Format.Encoding
dataPageHeaderEncoding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
        Core.projectionField = (Core.Name "encoding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderNumValues :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Int
dataPageHeaderNumValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
        Core.projectionField = (Core.Name "numValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderRepetitionLevelEncoding :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Format.Encoding
dataPageHeaderRepetitionLevelEncoding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
        Core.projectionField = (Core.Name "repetitionLevelEncoding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderStatistics :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm (Maybe Format.Statistics)
dataPageHeaderStatistics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
        Core.projectionField = (Core.Name "statistics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2 :: Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm (Maybe Format.Statistics) -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2 numValues numNulls numRows encoding definitionLevelsByteLength repetitionLevelsByteLength isCompressed statistics =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm numValues)},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Phantoms.unTTerm numNulls)},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Phantoms.unTTerm numRows)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm encoding)},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Phantoms.unTTerm definitionLevelsByteLength)},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Phantoms.unTTerm repetitionLevelsByteLength)},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Phantoms.unTTerm isCompressed)},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Phantoms.unTTerm statistics)}]}))

dataPageHeaderV2DefinitionLevelsByteLength :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int
dataPageHeaderV2DefinitionLevelsByteLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2Encoding :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Format.Encoding
dataPageHeaderV2Encoding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "encoding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2IsCompressed :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm (Maybe Bool)
dataPageHeaderV2IsCompressed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "isCompressed")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2NumNulls :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int
dataPageHeaderV2NumNulls x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "numNulls")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2NumRows :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int
dataPageHeaderV2NumRows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "numRows")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2NumValues :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int
dataPageHeaderV2NumValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "numValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2RepetitionLevelsByteLength :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int
dataPageHeaderV2RepetitionLevelsByteLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2Statistics :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm (Maybe Format.Statistics)
dataPageHeaderV2Statistics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
        Core.projectionField = (Core.Name "statistics")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dataPageHeaderV2WithDefinitionLevelsByteLength :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithDefinitionLevelsByteLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithEncoding :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithEncoding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithIsCompressed :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithIsCompressed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithNumNulls :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithNumNulls original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithNumRows :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithNumRows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithNumValues :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithNumValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithRepetitionLevelsByteLength :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithRepetitionLevelsByteLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderV2WithStatistics :: Phantoms.TTerm Format.DataPageHeaderV2 -> Phantoms.TTerm (Maybe Format.Statistics) -> Phantoms.TTerm Format.DataPageHeaderV2
dataPageHeaderV2WithStatistics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numNulls"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numNulls")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "definitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelsByteLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "repetitionLevelsByteLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isCompressed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeaderV2"),
              Core.projectionField = (Core.Name "isCompressed")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dataPageHeaderWithDefinitionLevelEncoding :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.DataPageHeader
dataPageHeaderWithDefinitionLevelEncoding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelEncoding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "repetitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderWithEncoding :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.DataPageHeader
dataPageHeaderWithEncoding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "definitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "repetitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderWithNumValues :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DataPageHeader
dataPageHeaderWithNumValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "definitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "repetitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderWithRepetitionLevelEncoding :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.DataPageHeader
dataPageHeaderWithRepetitionLevelEncoding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "definitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelEncoding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "statistics")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dataPageHeaderWithStatistics :: Phantoms.TTerm Format.DataPageHeader -> Phantoms.TTerm (Maybe Format.Statistics) -> Phantoms.TTerm Format.DataPageHeader
dataPageHeaderWithStatistics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "definitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionLevelEncoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DataPageHeader"),
              Core.projectionField = (Core.Name "repetitionLevelEncoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statistics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decimalType :: Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DecimalType
decimalType scale precision =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scale"),
          Core.fieldTerm = (Phantoms.unTTerm scale)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Phantoms.unTTerm precision)}]}))

decimalTypePrecision :: Phantoms.TTerm Format.DecimalType -> Phantoms.TTerm Int
decimalTypePrecision x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
        Core.projectionField = (Core.Name "precision")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decimalTypeScale :: Phantoms.TTerm Format.DecimalType -> Phantoms.TTerm Int
decimalTypeScale x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
        Core.projectionField = (Core.Name "scale")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

decimalTypeWithPrecision :: Phantoms.TTerm Format.DecimalType -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DecimalType
decimalTypeWithPrecision original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scale"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
              Core.projectionField = (Core.Name "scale")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

decimalTypeWithScale :: Phantoms.TTerm Format.DecimalType -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DecimalType
decimalTypeWithScale original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "scale"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precision"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DecimalType"),
              Core.projectionField = (Core.Name "precision")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dictionaryPageHeader :: Phantoms.TTerm Int -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.DictionaryPageHeader
dictionaryPageHeader numValues encoding isSorted =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm numValues)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm encoding)},
        Core.Field {
          Core.fieldName = (Core.Name "isSorted"),
          Core.fieldTerm = (Phantoms.unTTerm isSorted)}]}))

dictionaryPageHeaderEncoding :: Phantoms.TTerm Format.DictionaryPageHeader -> Phantoms.TTerm Format.Encoding
dictionaryPageHeaderEncoding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
        Core.projectionField = (Core.Name "encoding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dictionaryPageHeaderIsSorted :: Phantoms.TTerm Format.DictionaryPageHeader -> Phantoms.TTerm (Maybe Bool)
dictionaryPageHeaderIsSorted x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
        Core.projectionField = (Core.Name "isSorted")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dictionaryPageHeaderNumValues :: Phantoms.TTerm Format.DictionaryPageHeader -> Phantoms.TTerm Int
dictionaryPageHeaderNumValues x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
        Core.projectionField = (Core.Name "numValues")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

dictionaryPageHeaderWithEncoding :: Phantoms.TTerm Format.DictionaryPageHeader -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.DictionaryPageHeader
dictionaryPageHeaderWithEncoding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isSorted"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
              Core.projectionField = (Core.Name "isSorted")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

dictionaryPageHeaderWithIsSorted :: Phantoms.TTerm Format.DictionaryPageHeader -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Format.DictionaryPageHeader
dictionaryPageHeaderWithIsSorted original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
              Core.projectionField = (Core.Name "numValues")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isSorted"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

dictionaryPageHeaderWithNumValues :: Phantoms.TTerm Format.DictionaryPageHeader -> Phantoms.TTerm Int -> Phantoms.TTerm Format.DictionaryPageHeader
dictionaryPageHeaderWithNumValues original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "numValues"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isSorted"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.DictionaryPageHeader"),
              Core.projectionField = (Core.Name "isSorted")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

encodingBitPacked :: Phantoms.TTerm Format.Encoding
encodingBitPacked =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitPacked"),
        Core.fieldTerm = Core.TermUnit}}))

encodingByteStreamSplit :: Phantoms.TTerm Format.Encoding
encodingByteStreamSplit =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byteStreamSplit"),
        Core.fieldTerm = Core.TermUnit}}))

encodingDeltaBinaryPacked :: Phantoms.TTerm Format.Encoding
encodingDeltaBinaryPacked =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deltaBinaryPacked"),
        Core.fieldTerm = Core.TermUnit}}))

encodingDeltaByteArray :: Phantoms.TTerm Format.Encoding
encodingDeltaByteArray =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deltaByteArray"),
        Core.fieldTerm = Core.TermUnit}}))

encodingDeltaLengthByteArray :: Phantoms.TTerm Format.Encoding
encodingDeltaLengthByteArray =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deltaLengthByteArray"),
        Core.fieldTerm = Core.TermUnit}}))

encodingPlain :: Phantoms.TTerm Format.Encoding
encodingPlain =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plain"),
        Core.fieldTerm = Core.TermUnit}}))

encodingRle :: Phantoms.TTerm Format.Encoding
encodingRle =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rle"),
        Core.fieldTerm = Core.TermUnit}}))

encodingRleDictionary :: Phantoms.TTerm Format.Encoding
encodingRleDictionary =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Encoding"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rleDictionary"),
        Core.fieldTerm = Core.TermUnit}}))

encryptionAlgorithmAesGcmCtrV1 :: Phantoms.TTerm Format.AesGcmCtrV1 -> Phantoms.TTerm Format.EncryptionAlgorithm
encryptionAlgorithmAesGcmCtrV1 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.EncryptionAlgorithm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aesGcmCtrV1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

encryptionAlgorithmAesGcmV1 :: Phantoms.TTerm Format.AesGcmV1 -> Phantoms.TTerm Format.EncryptionAlgorithm
encryptionAlgorithmAesGcmV1 x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.EncryptionAlgorithm"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aesGcmV1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

encryptionWithColumnKey :: Phantoms.TTerm [String] -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.EncryptionWithColumnKey
encryptionWithColumnKey pathInSchema keyMetadata =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Phantoms.unTTerm pathInSchema)},
        Core.Field {
          Core.fieldName = (Core.Name "keyMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm keyMetadata)}]}))

encryptionWithColumnKeyKeyMetadata :: Phantoms.TTerm Format.EncryptionWithColumnKey -> Phantoms.TTerm (Maybe B.ByteString)
encryptionWithColumnKeyKeyMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
        Core.projectionField = (Core.Name "keyMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

encryptionWithColumnKeyPathInSchema :: Phantoms.TTerm Format.EncryptionWithColumnKey -> Phantoms.TTerm [String]
encryptionWithColumnKeyPathInSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
        Core.projectionField = (Core.Name "pathInSchema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

encryptionWithColumnKeyWithKeyMetadata :: Phantoms.TTerm Format.EncryptionWithColumnKey -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.EncryptionWithColumnKey
encryptionWithColumnKeyWithKeyMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
              Core.projectionField = (Core.Name "pathInSchema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

encryptionWithColumnKeyWithPathInSchema :: Phantoms.TTerm Format.EncryptionWithColumnKey -> Phantoms.TTerm [String] -> Phantoms.TTerm Format.EncryptionWithColumnKey
encryptionWithColumnKeyWithPathInSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathInSchema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.EncryptionWithColumnKey"),
              Core.projectionField = (Core.Name "keyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

encryptionWithFooterKey :: Phantoms.TTerm Format.EncryptionWithFooterKey
encryptionWithFooterKey =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.EncryptionWithFooterKey"),
      Core.recordFields = []}))

fieldRepetitionTypeOptional :: Phantoms.TTerm Format.FieldRepetitionType
fieldRepetitionTypeOptional =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.FieldRepetitionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = Core.TermUnit}}))

fieldRepetitionTypeRepeated :: Phantoms.TTerm Format.FieldRepetitionType
fieldRepetitionTypeRepeated =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.FieldRepetitionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "repeated"),
        Core.fieldTerm = Core.TermUnit}}))

fieldRepetitionTypeRequired :: Phantoms.TTerm Format.FieldRepetitionType
fieldRepetitionTypeRequired =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.FieldRepetitionType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "required"),
        Core.fieldTerm = Core.TermUnit}}))

fileCryptoMetaData :: Phantoms.TTerm Format.EncryptionAlgorithm -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.FileCryptoMetaData
fileCryptoMetaData encryptionAlgorithm keyMetadata =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Phantoms.unTTerm encryptionAlgorithm)},
        Core.Field {
          Core.fieldName = (Core.Name "keyMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm keyMetadata)}]}))

fileCryptoMetaDataEncryptionAlgorithm :: Phantoms.TTerm Format.FileCryptoMetaData -> Phantoms.TTerm Format.EncryptionAlgorithm
fileCryptoMetaDataEncryptionAlgorithm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
        Core.projectionField = (Core.Name "encryptionAlgorithm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileCryptoMetaDataKeyMetadata :: Phantoms.TTerm Format.FileCryptoMetaData -> Phantoms.TTerm (Maybe B.ByteString)
fileCryptoMetaDataKeyMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
        Core.projectionField = (Core.Name "keyMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileCryptoMetaDataWithEncryptionAlgorithm :: Phantoms.TTerm Format.FileCryptoMetaData -> Phantoms.TTerm Format.EncryptionAlgorithm -> Phantoms.TTerm Format.FileCryptoMetaData
fileCryptoMetaDataWithEncryptionAlgorithm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
              Core.projectionField = (Core.Name "keyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileCryptoMetaDataWithKeyMetadata :: Phantoms.TTerm Format.FileCryptoMetaData -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.FileCryptoMetaData
fileCryptoMetaDataWithKeyMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileCryptoMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fileMetaData :: Phantoms.TTerm Int -> Phantoms.TTerm [Format.SchemaElement] -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm [Format.RowGroup] -> Phantoms.TTerm (Maybe [Format.KeyValue]) -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm (Maybe [Format.ColumnOrder]) -> Phantoms.TTerm (Maybe Format.EncryptionAlgorithm) -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.FileMetaData
fileMetaData version schema numRows rowGroups keyValueMetadata createdBy columnOrders encryptionAlgorithm footerSigningKeyMetadata =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm version)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm schema)},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Phantoms.unTTerm numRows)},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Phantoms.unTTerm rowGroups)},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm keyValueMetadata)},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Phantoms.unTTerm createdBy)},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Phantoms.unTTerm columnOrders)},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Phantoms.unTTerm encryptionAlgorithm)},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm footerSigningKeyMetadata)}]}))

fileMetaDataColumnOrders :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe [Format.ColumnOrder])
fileMetaDataColumnOrders x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "columnOrders")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataCreatedBy :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe String)
fileMetaDataCreatedBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "createdBy")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataEncryptionAlgorithm :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe Format.EncryptionAlgorithm)
fileMetaDataEncryptionAlgorithm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "encryptionAlgorithm")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataFooterSigningKeyMetadata :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe B.ByteString)
fileMetaDataFooterSigningKeyMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataKeyValueMetadata :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe [Format.KeyValue])
fileMetaDataKeyValueMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "keyValueMetadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataNumRows :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm I.Int64
fileMetaDataNumRows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "numRows")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataRowGroups :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm [Format.RowGroup]
fileMetaDataRowGroups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "rowGroups")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataSchema :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm [Format.SchemaElement]
fileMetaDataSchema x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "schema")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataVersion :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm Int
fileMetaDataVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
        Core.projectionField = (Core.Name "version")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

fileMetaDataWithColumnOrders :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe [Format.ColumnOrder]) -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithColumnOrders original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithCreatedBy :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithCreatedBy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithEncryptionAlgorithm :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe Format.EncryptionAlgorithm) -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithEncryptionAlgorithm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithFooterSigningKeyMetadata :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithFooterSigningKeyMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

fileMetaDataWithKeyValueMetadata :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm (Maybe [Format.KeyValue]) -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithKeyValueMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithNumRows :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithNumRows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithRowGroups :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm [Format.RowGroup] -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithRowGroups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithSchema :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm [Format.SchemaElement] -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithSchema original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

fileMetaDataWithVersion :: Phantoms.TTerm Format.FileMetaData -> Phantoms.TTerm Int -> Phantoms.TTerm Format.FileMetaData
fileMetaDataWithVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schema"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "schema")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rowGroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "rowGroups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "keyValueMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "keyValueMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createdBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "createdBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "columnOrders"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "columnOrders")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encryptionAlgorithm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "encryptionAlgorithm")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "footerSigningKeyMetadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.FileMetaData"),
              Core.projectionField = (Core.Name "footerSigningKeyMetadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

indexPageHeader :: Phantoms.TTerm Format.IndexPageHeader
indexPageHeader =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.IndexPageHeader"),
      Core.recordFields = []}))

intType :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.IntType
intType bitWidth isSigned =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.IntType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bitWidth"),
          Core.fieldTerm = (Phantoms.unTTerm bitWidth)},
        Core.Field {
          Core.fieldName = (Core.Name "isSigned"),
          Core.fieldTerm = (Phantoms.unTTerm isSigned)}]}))

intTypeBitWidth :: Phantoms.TTerm Format.IntType -> Phantoms.TTerm I.Int16
intTypeBitWidth x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.IntType"),
        Core.projectionField = (Core.Name "bitWidth")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

intTypeIsSigned :: Phantoms.TTerm Format.IntType -> Phantoms.TTerm Bool
intTypeIsSigned x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.IntType"),
        Core.projectionField = (Core.Name "isSigned")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

intTypeWithBitWidth :: Phantoms.TTerm Format.IntType -> Phantoms.TTerm I.Int16 -> Phantoms.TTerm Format.IntType
intTypeWithBitWidth original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.IntType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bitWidth"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isSigned"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.IntType"),
              Core.projectionField = (Core.Name "isSigned")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

intTypeWithIsSigned :: Phantoms.TTerm Format.IntType -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.IntType
intTypeWithIsSigned original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.IntType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bitWidth"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.IntType"),
              Core.projectionField = (Core.Name "bitWidth")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isSigned"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

keyValue :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Format.KeyValue
keyValue key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

keyValueKey :: Phantoms.TTerm Format.KeyValue -> Phantoms.TTerm String
keyValueKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValueValue :: Phantoms.TTerm Format.KeyValue -> Phantoms.TTerm (Maybe String)
keyValueValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValueWithKey :: Phantoms.TTerm Format.KeyValue -> Phantoms.TTerm String -> Phantoms.TTerm Format.KeyValue
keyValueWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

keyValueWithValue :: Phantoms.TTerm Format.KeyValue -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Format.KeyValue
keyValueWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.KeyValue"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

logicalTypeBson :: Phantoms.TTerm Format.LogicalType
logicalTypeBson =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bson"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeDate :: Phantoms.TTerm Format.LogicalType
logicalTypeDate =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeDecimal :: Phantoms.TTerm Format.DecimalType -> Phantoms.TTerm Format.LogicalType
logicalTypeDecimal x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalTypeEnum :: Phantoms.TTerm Format.LogicalType
logicalTypeEnum =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "enum"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeInteger :: Phantoms.TTerm Format.IntType -> Phantoms.TTerm Format.LogicalType
logicalTypeInteger x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalTypeJson :: Phantoms.TTerm Format.LogicalType
logicalTypeJson =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "json"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeList :: Phantoms.TTerm Format.LogicalType
logicalTypeList =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeMap :: Phantoms.TTerm Format.LogicalType
logicalTypeMap =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeString :: Phantoms.TTerm Format.LogicalType
logicalTypeString =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeTime :: Phantoms.TTerm Format.TimeType -> Phantoms.TTerm Format.LogicalType
logicalTypeTime x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "time"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalTypeTimestamp :: Phantoms.TTerm Format.TimestampType -> Phantoms.TTerm Format.LogicalType
logicalTypeTimestamp x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "timestamp"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

logicalTypeUnknown :: Phantoms.TTerm Format.LogicalType
logicalTypeUnknown =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unknown"),
        Core.fieldTerm = Core.TermUnit}}))

logicalTypeUuid :: Phantoms.TTerm Format.LogicalType
logicalTypeUuid =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.LogicalType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "uuid"),
        Core.fieldTerm = Core.TermUnit}}))

offsetIndex :: Phantoms.TTerm [Format.PageLocation] -> Phantoms.TTerm Format.OffsetIndex
offsetIndex pageLocations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.OffsetIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pageLocations"),
          Core.fieldTerm = (Phantoms.unTTerm pageLocations)}]}))

offsetIndexPageLocations :: Phantoms.TTerm Format.OffsetIndex -> Phantoms.TTerm [Format.PageLocation]
offsetIndexPageLocations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.OffsetIndex"),
        Core.projectionField = (Core.Name "pageLocations")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

offsetIndexWithPageLocations :: Phantoms.TTerm Format.OffsetIndex -> Phantoms.TTerm [Format.PageLocation] -> Phantoms.TTerm Format.OffsetIndex
offsetIndexWithPageLocations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.OffsetIndex"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pageLocations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pageEncodingStats :: Phantoms.TTerm Format.PageType -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Int -> Phantoms.TTerm Format.PageEncodingStats
pageEncodingStats pageType encoding count =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pageType"),
          Core.fieldTerm = (Phantoms.unTTerm pageType)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm encoding)},
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Phantoms.unTTerm count)}]}))

pageEncodingStatsCount :: Phantoms.TTerm Format.PageEncodingStats -> Phantoms.TTerm Int
pageEncodingStatsCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
        Core.projectionField = (Core.Name "count")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageEncodingStatsEncoding :: Phantoms.TTerm Format.PageEncodingStats -> Phantoms.TTerm Format.Encoding
pageEncodingStatsEncoding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
        Core.projectionField = (Core.Name "encoding")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageEncodingStatsPageType :: Phantoms.TTerm Format.PageEncodingStats -> Phantoms.TTerm Format.PageType
pageEncodingStatsPageType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
        Core.projectionField = (Core.Name "pageType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageEncodingStatsWithCount :: Phantoms.TTerm Format.PageEncodingStats -> Phantoms.TTerm Int -> Phantoms.TTerm Format.PageEncodingStats
pageEncodingStatsWithCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pageType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
              Core.projectionField = (Core.Name "pageType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pageEncodingStatsWithEncoding :: Phantoms.TTerm Format.PageEncodingStats -> Phantoms.TTerm Format.Encoding -> Phantoms.TTerm Format.PageEncodingStats
pageEncodingStatsWithEncoding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pageType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
              Core.projectionField = (Core.Name "pageType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
              Core.projectionField = (Core.Name "count")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageEncodingStatsWithPageType :: Phantoms.TTerm Format.PageEncodingStats -> Phantoms.TTerm Format.PageType -> Phantoms.TTerm Format.PageEncodingStats
pageEncodingStatsWithPageType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pageType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encoding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
              Core.projectionField = (Core.Name "encoding")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageEncodingStats"),
              Core.projectionField = (Core.Name "count")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeader :: Phantoms.TTerm Format.PageType -> Phantoms.TTerm Int -> Phantoms.TTerm Int -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm (Maybe Format.DataPageHeader) -> Phantoms.TTerm (Maybe Format.IndexPageHeader) -> Phantoms.TTerm (Maybe Format.DictionaryPageHeader) -> Phantoms.TTerm (Maybe Format.DataPageHeaderV2) -> Phantoms.TTerm Format.PageHeader
pageHeader type_ uncompressedPageSize compressedPageSize crc dataPageHeader indexPageHeader dictionaryPageHeader dataPageHeaderV2 =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Phantoms.unTTerm uncompressedPageSize)},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Phantoms.unTTerm compressedPageSize)},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Phantoms.unTTerm crc)},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Phantoms.unTTerm dataPageHeader)},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Phantoms.unTTerm indexPageHeader)},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Phantoms.unTTerm dictionaryPageHeader)},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Phantoms.unTTerm dataPageHeaderV2)}]}))

pageHeaderCompressedPageSize :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm Int
pageHeaderCompressedPageSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "compressedPageSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderCrc :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Int)
pageHeaderCrc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "crc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderDataPageHeader :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.DataPageHeader)
pageHeaderDataPageHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "dataPageHeader")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderDataPageHeaderV2 :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.DataPageHeaderV2)
pageHeaderDataPageHeaderV2 x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "dataPageHeaderV2")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderDictionaryPageHeader :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.DictionaryPageHeader)
pageHeaderDictionaryPageHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "dictionaryPageHeader")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderIndexPageHeader :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.IndexPageHeader)
pageHeaderIndexPageHeader x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "indexPageHeader")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderType :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm Format.PageType
pageHeaderType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderUncompressedPageSize :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm Int
pageHeaderUncompressedPageSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
        Core.projectionField = (Core.Name "uncompressedPageSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageHeaderWithCompressedPageSize :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm Int -> Phantoms.TTerm Format.PageHeader
pageHeaderWithCompressedPageSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeaderWithCrc :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Format.PageHeader
pageHeaderWithCrc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeaderWithDataPageHeader :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.DataPageHeader) -> Phantoms.TTerm Format.PageHeader
pageHeaderWithDataPageHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeaderWithDataPageHeaderV2 :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.DataPageHeaderV2) -> Phantoms.TTerm Format.PageHeader
pageHeaderWithDataPageHeaderV2 original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pageHeaderWithDictionaryPageHeader :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.DictionaryPageHeader) -> Phantoms.TTerm Format.PageHeader
pageHeaderWithDictionaryPageHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeaderWithIndexPageHeader :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm (Maybe Format.IndexPageHeader) -> Phantoms.TTerm Format.PageHeader
pageHeaderWithIndexPageHeader original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeaderWithType :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm Format.PageType -> Phantoms.TTerm Format.PageHeader
pageHeaderWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "uncompressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageHeaderWithUncompressedPageSize :: Phantoms.TTerm Format.PageHeader -> Phantoms.TTerm Int -> Phantoms.TTerm Format.PageHeader
pageHeaderWithUncompressedPageSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "uncompressedPageSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "crc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "crc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "indexPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "indexPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dictionaryPageHeader"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dictionaryPageHeader")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dataPageHeaderV2"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageHeader"),
              Core.projectionField = (Core.Name "dataPageHeaderV2")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageLocation :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Int -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.PageLocation
pageLocation offset compressedPageSize firstRowIndex =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Phantoms.unTTerm offset)},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Phantoms.unTTerm compressedPageSize)},
        Core.Field {
          Core.fieldName = (Core.Name "firstRowIndex"),
          Core.fieldTerm = (Phantoms.unTTerm firstRowIndex)}]}))

pageLocationCompressedPageSize :: Phantoms.TTerm Format.PageLocation -> Phantoms.TTerm Int
pageLocationCompressedPageSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
        Core.projectionField = (Core.Name "compressedPageSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageLocationFirstRowIndex :: Phantoms.TTerm Format.PageLocation -> Phantoms.TTerm I.Int64
pageLocationFirstRowIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
        Core.projectionField = (Core.Name "firstRowIndex")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageLocationOffset :: Phantoms.TTerm Format.PageLocation -> Phantoms.TTerm I.Int64
pageLocationOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
        Core.projectionField = (Core.Name "offset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pageLocationWithCompressedPageSize :: Phantoms.TTerm Format.PageLocation -> Phantoms.TTerm Int -> Phantoms.TTerm Format.PageLocation
pageLocationWithCompressedPageSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
              Core.projectionField = (Core.Name "offset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "firstRowIndex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
              Core.projectionField = (Core.Name "firstRowIndex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageLocationWithFirstRowIndex :: Phantoms.TTerm Format.PageLocation -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.PageLocation
pageLocationWithFirstRowIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
              Core.projectionField = (Core.Name "offset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "firstRowIndex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pageLocationWithOffset :: Phantoms.TTerm Format.PageLocation -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.PageLocation
pageLocationWithOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "offset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "compressedPageSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
              Core.projectionField = (Core.Name "compressedPageSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "firstRowIndex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.PageLocation"),
              Core.projectionField = (Core.Name "firstRowIndex")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pageTypeDataPage :: Phantoms.TTerm Format.PageType
pageTypeDataPage =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.PageType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPage"),
        Core.fieldTerm = Core.TermUnit}}))

pageTypeDataPageV2 :: Phantoms.TTerm Format.PageType
pageTypeDataPageV2 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.PageType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataPageV2"),
        Core.fieldTerm = Core.TermUnit}}))

pageTypeDictionaryPage :: Phantoms.TTerm Format.PageType
pageTypeDictionaryPage =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.PageType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dictionaryPage"),
        Core.fieldTerm = Core.TermUnit}}))

pageTypeIndexPage :: Phantoms.TTerm Format.PageType
pageTypeIndexPage =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.PageType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indexPage"),
        Core.fieldTerm = Core.TermUnit}}))

rowGroup :: Phantoms.TTerm [Format.ColumnChunk] -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm (Maybe [Format.SortingColumn]) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm (Maybe I.Int16) -> Phantoms.TTerm Format.RowGroup
rowGroup columns totalByteSize numRows sortingColumns fileOffset totalCompressedSize ordinal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm columns)},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Phantoms.unTTerm totalByteSize)},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Phantoms.unTTerm numRows)},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Phantoms.unTTerm sortingColumns)},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Phantoms.unTTerm fileOffset)},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Phantoms.unTTerm totalCompressedSize)},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Phantoms.unTTerm ordinal)}]}))

rowGroupColumns :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm [Format.ColumnChunk]
rowGroupColumns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "columns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupFileOffset :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe I.Int64)
rowGroupFileOffset x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "fileOffset")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupNumRows :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm I.Int64
rowGroupNumRows x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "numRows")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupOrdinal :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe I.Int16)
rowGroupOrdinal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "ordinal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupSortingColumns :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe [Format.SortingColumn])
rowGroupSortingColumns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "sortingColumns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupTotalByteSize :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm I.Int64
rowGroupTotalByteSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "totalByteSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupTotalCompressedSize :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe I.Int64)
rowGroupTotalCompressedSize x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
        Core.projectionField = (Core.Name "totalCompressedSize")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rowGroupWithColumns :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm [Format.ColumnChunk] -> Phantoms.TTerm Format.RowGroup
rowGroupWithColumns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalByteSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "sortingColumns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "ordinal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rowGroupWithFileOffset :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.RowGroup
rowGroupWithFileOffset original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalByteSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "sortingColumns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "ordinal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rowGroupWithNumRows :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.RowGroup
rowGroupWithNumRows original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalByteSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "sortingColumns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "ordinal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rowGroupWithOrdinal :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe I.Int16) -> Phantoms.TTerm Format.RowGroup
rowGroupWithOrdinal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalByteSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "sortingColumns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rowGroupWithSortingColumns :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe [Format.SortingColumn]) -> Phantoms.TTerm Format.RowGroup
rowGroupWithSortingColumns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalByteSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "ordinal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rowGroupWithTotalByteSize :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm I.Int64 -> Phantoms.TTerm Format.RowGroup
rowGroupWithTotalByteSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "sortingColumns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalCompressedSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "ordinal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rowGroupWithTotalCompressedSize :: Phantoms.TTerm Format.RowGroup -> Phantoms.TTerm (Maybe I.Int64) -> Phantoms.TTerm Format.RowGroup
rowGroupWithTotalCompressedSize original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalByteSize"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "totalByteSize")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numRows"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "numRows")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sortingColumns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "sortingColumns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileOffset"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "fileOffset")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "totalCompressedSize"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ordinal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.RowGroup"),
              Core.projectionField = (Core.Name "ordinal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaElement :: Phantoms.TTerm (Maybe Format.Type) -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm (Maybe Format.FieldRepetitionType) -> Phantoms.TTerm String -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm (Maybe Format.LogicalType) -> Phantoms.TTerm Format.SchemaElement
schemaElement type_ typeLength repetitionType name numChildren fieldId logicalType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Phantoms.unTTerm typeLength)},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Phantoms.unTTerm repetitionType)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Phantoms.unTTerm numChildren)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Phantoms.unTTerm fieldId)},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Phantoms.unTTerm logicalType)}]}))

schemaElementFieldId :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Int)
schemaElementFieldId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "fieldId")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementLogicalType :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Format.LogicalType)
schemaElementLogicalType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "logicalType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementName :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm String
schemaElementName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementNumChildren :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Int)
schemaElementNumChildren x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "numChildren")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementRepetitionType :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Format.FieldRepetitionType)
schemaElementRepetitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "repetitionType")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementType :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Format.Type)
schemaElementType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementTypeLength :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Int)
schemaElementTypeLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
        Core.projectionField = (Core.Name "typeLength")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

schemaElementWithFieldId :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Format.SchemaElement
schemaElementWithFieldId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "typeLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "repetitionType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "numChildren")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "logicalType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaElementWithLogicalType :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Format.LogicalType) -> Phantoms.TTerm Format.SchemaElement
schemaElementWithLogicalType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "typeLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "repetitionType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "numChildren")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "fieldId")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

schemaElementWithName :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm String -> Phantoms.TTerm Format.SchemaElement
schemaElementWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "typeLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "repetitionType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "numChildren")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "fieldId")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "logicalType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaElementWithNumChildren :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Format.SchemaElement
schemaElementWithNumChildren original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "typeLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "repetitionType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "fieldId")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "logicalType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaElementWithRepetitionType :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Format.FieldRepetitionType) -> Phantoms.TTerm Format.SchemaElement
schemaElementWithRepetitionType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "typeLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "numChildren")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "fieldId")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "logicalType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaElementWithType :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Format.Type) -> Phantoms.TTerm Format.SchemaElement
schemaElementWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "typeLength")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "repetitionType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "numChildren")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "fieldId")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "logicalType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

schemaElementWithTypeLength :: Phantoms.TTerm Format.SchemaElement -> Phantoms.TTerm (Maybe Int) -> Phantoms.TTerm Format.SchemaElement
schemaElementWithTypeLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeLength"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "repetitionType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "repetitionType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "numChildren"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "numChildren")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fieldId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "fieldId")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "logicalType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SchemaElement"),
              Core.projectionField = (Core.Name "logicalType")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sortingColumn :: Phantoms.TTerm Int -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.SortingColumn
sortingColumn columnIdx descending nullsFirst =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columnIdx"),
          Core.fieldTerm = (Phantoms.unTTerm columnIdx)},
        Core.Field {
          Core.fieldName = (Core.Name "descending"),
          Core.fieldTerm = (Phantoms.unTTerm descending)},
        Core.Field {
          Core.fieldName = (Core.Name "nullsFirst"),
          Core.fieldTerm = (Phantoms.unTTerm nullsFirst)}]}))

sortingColumnColumnIdx :: Phantoms.TTerm Format.SortingColumn -> Phantoms.TTerm Int
sortingColumnColumnIdx x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
        Core.projectionField = (Core.Name "columnIdx")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortingColumnDescending :: Phantoms.TTerm Format.SortingColumn -> Phantoms.TTerm Bool
sortingColumnDescending x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
        Core.projectionField = (Core.Name "descending")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortingColumnNullsFirst :: Phantoms.TTerm Format.SortingColumn -> Phantoms.TTerm Bool
sortingColumnNullsFirst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
        Core.projectionField = (Core.Name "nullsFirst")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortingColumnWithColumnIdx :: Phantoms.TTerm Format.SortingColumn -> Phantoms.TTerm Int -> Phantoms.TTerm Format.SortingColumn
sortingColumnWithColumnIdx original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columnIdx"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "descending"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
              Core.projectionField = (Core.Name "descending")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullsFirst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
              Core.projectionField = (Core.Name "nullsFirst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sortingColumnWithDescending :: Phantoms.TTerm Format.SortingColumn -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.SortingColumn
sortingColumnWithDescending original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columnIdx"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
              Core.projectionField = (Core.Name "columnIdx")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "descending"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "nullsFirst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
              Core.projectionField = (Core.Name "nullsFirst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sortingColumnWithNullsFirst :: Phantoms.TTerm Format.SortingColumn -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.SortingColumn
sortingColumnWithNullsFirst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columnIdx"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
              Core.projectionField = (Core.Name "columnIdx")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "descending"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.SortingColumn"),
              Core.projectionField = (Core.Name "descending")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullsFirst"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

statistics :: Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.Statistics
statistics nullCount distinctCount maxValue minValue =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.Statistics"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullCount"),
          Core.fieldTerm = (Phantoms.unTTerm nullCount)},
        Core.Field {
          Core.fieldName = (Core.Name "distinctCount"),
          Core.fieldTerm = (Phantoms.unTTerm distinctCount)},
        Core.Field {
          Core.fieldName = (Core.Name "maxValue"),
          Core.fieldTerm = (Phantoms.unTTerm maxValue)},
        Core.Field {
          Core.fieldName = (Core.Name "minValue"),
          Core.fieldTerm = (Phantoms.unTTerm minValue)}]}))

statisticsDistinctCount :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe Integer)
statisticsDistinctCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
        Core.projectionField = (Core.Name "distinctCount")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statisticsMaxValue :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe B.ByteString)
statisticsMaxValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
        Core.projectionField = (Core.Name "maxValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statisticsMinValue :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe B.ByteString)
statisticsMinValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
        Core.projectionField = (Core.Name "minValue")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statisticsNullCount :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe Integer)
statisticsNullCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
        Core.projectionField = (Core.Name "nullCount")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statisticsWithDistinctCount :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm Format.Statistics
statisticsWithDistinctCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.Statistics"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "nullCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinctCount"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "maxValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "maxValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "minValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

statisticsWithMaxValue :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.Statistics
statisticsWithMaxValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.Statistics"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "nullCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinctCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "distinctCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "minValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "minValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

statisticsWithMinValue :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe B.ByteString) -> Phantoms.TTerm Format.Statistics
statisticsWithMinValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.Statistics"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "nullCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinctCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "distinctCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "maxValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValue"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

statisticsWithNullCount :: Phantoms.TTerm Format.Statistics -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm Format.Statistics
statisticsWithNullCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.Statistics"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nullCount"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "distinctCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "distinctCount")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "maxValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "maxValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "minValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.Statistics"),
              Core.projectionField = (Core.Name "minValue")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

timeType :: Phantoms.TTerm Bool -> Phantoms.TTerm Format.TimeUnit -> Phantoms.TTerm Format.TimeType
timeType isAdjustedToUtc unit =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.TimeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isAdjustedToUtc"),
          Core.fieldTerm = (Phantoms.unTTerm isAdjustedToUtc)},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm unit)}]}))

timeTypeIsAdjustedToUtc :: Phantoms.TTerm Format.TimeType -> Phantoms.TTerm Bool
timeTypeIsAdjustedToUtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimeType"),
        Core.projectionField = (Core.Name "isAdjustedToUtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timeTypeUnit :: Phantoms.TTerm Format.TimeType -> Phantoms.TTerm Format.TimeUnit
timeTypeUnit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimeType"),
        Core.projectionField = (Core.Name "unit")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timeTypeWithIsAdjustedToUtc :: Phantoms.TTerm Format.TimeType -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.TimeType
timeTypeWithIsAdjustedToUtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.TimeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isAdjustedToUtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimeType"),
              Core.projectionField = (Core.Name "unit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

timeTypeWithUnit :: Phantoms.TTerm Format.TimeType -> Phantoms.TTerm Format.TimeUnit -> Phantoms.TTerm Format.TimeType
timeTypeWithUnit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.TimeType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isAdjustedToUtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimeType"),
              Core.projectionField = (Core.Name "isAdjustedToUtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

timeUnitMicros :: Phantoms.TTerm Format.TimeUnit
timeUnitMicros =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.TimeUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "micros"),
        Core.fieldTerm = Core.TermUnit}}))

timeUnitMillis :: Phantoms.TTerm Format.TimeUnit
timeUnitMillis =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.TimeUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "millis"),
        Core.fieldTerm = Core.TermUnit}}))

timeUnitNanos :: Phantoms.TTerm Format.TimeUnit
timeUnitNanos =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.TimeUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nanos"),
        Core.fieldTerm = Core.TermUnit}}))

timestampType :: Phantoms.TTerm Bool -> Phantoms.TTerm Format.TimeUnit -> Phantoms.TTerm Format.TimestampType
timestampType isAdjustedToUtc unit =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isAdjustedToUtc"),
          Core.fieldTerm = (Phantoms.unTTerm isAdjustedToUtc)},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm unit)}]}))

timestampTypeIsAdjustedToUtc :: Phantoms.TTerm Format.TimestampType -> Phantoms.TTerm Bool
timestampTypeIsAdjustedToUtc x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
        Core.projectionField = (Core.Name "isAdjustedToUtc")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timestampTypeUnit :: Phantoms.TTerm Format.TimestampType -> Phantoms.TTerm Format.TimeUnit
timestampTypeUnit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
        Core.projectionField = (Core.Name "unit")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

timestampTypeWithIsAdjustedToUtc :: Phantoms.TTerm Format.TimestampType -> Phantoms.TTerm Bool -> Phantoms.TTerm Format.TimestampType
timestampTypeWithIsAdjustedToUtc original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isAdjustedToUtc"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
              Core.projectionField = (Core.Name "unit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

timestampTypeWithUnit :: Phantoms.TTerm Format.TimestampType -> Phantoms.TTerm Format.TimeUnit -> Phantoms.TTerm Format.TimestampType
timestampTypeWithUnit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isAdjustedToUtc"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.parquet.format.TimestampType"),
              Core.projectionField = (Core.Name "isAdjustedToUtc")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeBoolean :: Phantoms.TTerm Format.Type
typeBoolean =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

typeByteArray :: Phantoms.TTerm Format.Type
typeByteArray =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byteArray"),
        Core.fieldTerm = Core.TermUnit}}))

typeDouble :: Phantoms.TTerm Format.Type
typeDouble =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

typeFixedLenByteArray :: Phantoms.TTerm Format.Type
typeFixedLenByteArray =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fixedLenByteArray"),
        Core.fieldTerm = Core.TermUnit}}))

typeFloat :: Phantoms.TTerm Format.Type
typeFloat =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

typeInt32 :: Phantoms.TTerm Format.Type
typeInt32 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int32"),
        Core.fieldTerm = Core.TermUnit}}))

typeInt64 :: Phantoms.TTerm Format.Type
typeInt64 =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.parquet.format.Type"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int64"),
        Core.fieldTerm = Core.TermUnit}}))
