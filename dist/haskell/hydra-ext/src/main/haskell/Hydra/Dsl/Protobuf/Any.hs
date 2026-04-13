-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.protobuf.any

module Hydra.Dsl.Protobuf.Any where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Protobuf.Any as Any
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B

any :: Phantoms.TTerm String -> Phantoms.TTerm B.ByteString -> Phantoms.TTerm Any.Any
any typeUrl value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.protobuf.any.Any"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeUrl"),
          Core.fieldTerm = (Phantoms.unTTerm typeUrl)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

anyTypeUrl :: Phantoms.TTerm Any.Any -> Phantoms.TTerm String
anyTypeUrl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.protobuf.any.Any"),
        Core.projectionField = (Core.Name "typeUrl")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

anyValue :: Phantoms.TTerm Any.Any -> Phantoms.TTerm B.ByteString
anyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.protobuf.any.Any"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

anyWithTypeUrl :: Phantoms.TTerm Any.Any -> Phantoms.TTerm String -> Phantoms.TTerm Any.Any
anyWithTypeUrl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.protobuf.any.Any"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeUrl"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.protobuf.any.Any"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

anyWithValue :: Phantoms.TTerm Any.Any -> Phantoms.TTerm B.ByteString -> Phantoms.TTerm Any.Any
anyWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.protobuf.any.Any"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeUrl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.protobuf.any.Any"),
              Core.projectionField = (Core.Name "typeUrl")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
