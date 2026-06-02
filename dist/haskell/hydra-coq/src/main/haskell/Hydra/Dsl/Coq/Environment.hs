-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.coq.environment

module Hydra.Dsl.Coq.Environment where

import qualified Hydra.Coq.Environment as Environment
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

coqEnvironment :: Phantoms.TypedTerm String -> Phantoms.TypedTerm (M.Map String Int) -> Phantoms.TypedTerm (S.Set String) -> Phantoms.TypedTerm (S.Set String) -> Phantoms.TypedTerm Environment.CoqEnvironment
coqEnvironment currentNamespace constructorCounts ambiguousNames sanitizedAccessors =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTypedTerm currentNamespace)},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Phantoms.unTypedTerm constructorCounts)},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Phantoms.unTypedTerm ambiguousNames)},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Phantoms.unTypedTerm sanitizedAccessors)}]}))

coqEnvironmentAmbiguousNames :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm (S.Set String)
coqEnvironmentAmbiguousNames x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionFieldName = (Core.Name "ambiguousNames")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

coqEnvironmentConstructorCounts :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm (M.Map String Int)
coqEnvironmentConstructorCounts x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionFieldName = (Core.Name "constructorCounts")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

coqEnvironmentCurrentNamespace :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm String
coqEnvironmentCurrentNamespace x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionFieldName = (Core.Name "currentNamespace")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

coqEnvironmentSanitizedAccessors :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm (S.Set String)
coqEnvironmentSanitizedAccessors x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionFieldName = (Core.Name "sanitizedAccessors")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

coqEnvironmentWithAmbiguousNames :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm (S.Set String) -> Phantoms.TypedTerm Environment.CoqEnvironment
coqEnvironmentWithAmbiguousNames original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "constructorCounts")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "sanitizedAccessors")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

coqEnvironmentWithConstructorCounts :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm (M.Map String Int) -> Phantoms.TypedTerm Environment.CoqEnvironment
coqEnvironmentWithConstructorCounts original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "ambiguousNames")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "sanitizedAccessors")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

coqEnvironmentWithCurrentNamespace :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Environment.CoqEnvironment
coqEnvironmentWithCurrentNamespace original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "constructorCounts")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "ambiguousNames")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "sanitizedAccessors")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

coqEnvironmentWithSanitizedAccessors :: Phantoms.TypedTerm Environment.CoqEnvironment -> Phantoms.TypedTerm (S.Set String) -> Phantoms.TypedTerm Environment.CoqEnvironment
coqEnvironmentWithSanitizedAccessors original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "constructorCounts")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionFieldName = (Core.Name "ambiguousNames")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
