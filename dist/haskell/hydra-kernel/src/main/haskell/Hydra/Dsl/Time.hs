-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.time

module Hydra.Dsl.Time where

import qualified Hydra.Core as Core
import qualified Hydra.Decode.Time as DecodeTime
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Encode.Time as EncodeTime
import qualified Hydra.Time as Time
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I

-- | DSL constructor for hydra.time.Timespec
timespec :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm I.Int64 -> Typed.TypedTerm Time.Timespec
timespec seconds nanoseconds =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.time.Timespec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "seconds"),
          Core.fieldTerm = (Typed.unTypedTerm seconds)},
        Core.Field {
          Core.fieldName = (Core.Name "nanoseconds"),
          Core.fieldTerm = (Typed.unTypedTerm nanoseconds)}]}))

-- | DSL accessor for the nanoseconds field of hydra.time.Timespec
timespecNanoseconds :: Typed.TypedTerm Time.Timespec -> Typed.TypedTerm I.Int64
timespecNanoseconds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.time.Timespec"),
        Core.projectionFieldName = (Core.Name "nanoseconds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL accessor for the seconds field of hydra.time.Timespec
timespecSeconds :: Typed.TypedTerm Time.Timespec -> Typed.TypedTerm I.Int64
timespecSeconds x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.time.Timespec"),
        Core.projectionFieldName = (Core.Name "seconds")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))

-- | DSL name token for hydra.time.Timespec
timespecTimespec :: Typed.TypedName Time.Timespec
timespecTimespec = Typed.TypedName (Core.Name "hydra.time.Timespec")

-- | DSL updater for the nanoseconds field of hydra.time.Timespec
timespecWithNanoseconds :: Typed.TypedTerm Time.Timespec -> Typed.TypedTerm I.Int64 -> Typed.TypedTerm Time.Timespec
timespecWithNanoseconds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.time.Timespec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "seconds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.time.Timespec"),
              Core.projectionFieldName = (Core.Name "seconds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nanoseconds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))

-- | DSL updater for the seconds field of hydra.time.Timespec
timespecWithSeconds :: Typed.TypedTerm Time.Timespec -> Typed.TypedTerm I.Int64 -> Typed.TypedTerm Time.Timespec
timespecWithSeconds original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.time.Timespec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "seconds"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "nanoseconds"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.time.Timespec"),
              Core.projectionFieldName = (Core.Name "nanoseconds")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
