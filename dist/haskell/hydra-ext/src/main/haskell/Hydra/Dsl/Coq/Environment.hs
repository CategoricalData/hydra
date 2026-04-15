-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.coq.environment

module Hydra.Dsl.Coq.Environment where

import qualified Hydra.Coq.Environment as Environment
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

coqEnvironment :: Phantoms.TTerm String -> Phantoms.TTerm (M.Map String Int) -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm Environment.CoqEnvironment
coqEnvironment currentNamespace constructorCounts ambiguousNames sanitizedAccessors =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm currentNamespace)},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Phantoms.unTTerm constructorCounts)},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Phantoms.unTTerm ambiguousNames)},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Phantoms.unTTerm sanitizedAccessors)}]}))

coqEnvironmentAmbiguousNames :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm (S.Set String)
coqEnvironmentAmbiguousNames x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionField = (Core.Name "ambiguousNames")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coqEnvironmentConstructorCounts :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm (M.Map String Int)
coqEnvironmentConstructorCounts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionField = (Core.Name "constructorCounts")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coqEnvironmentCurrentNamespace :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm String
coqEnvironmentCurrentNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionField = (Core.Name "currentNamespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coqEnvironmentSanitizedAccessors :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm (S.Set String)
coqEnvironmentSanitizedAccessors x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
        Core.projectionField = (Core.Name "sanitizedAccessors")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

coqEnvironmentWithAmbiguousNames :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm Environment.CoqEnvironment
coqEnvironmentWithAmbiguousNames original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "constructorCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "sanitizedAccessors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

coqEnvironmentWithConstructorCounts :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm (M.Map String Int) -> Phantoms.TTerm Environment.CoqEnvironment
coqEnvironmentWithConstructorCounts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "ambiguousNames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "sanitizedAccessors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

coqEnvironmentWithCurrentNamespace :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm String -> Phantoms.TTerm Environment.CoqEnvironment
coqEnvironmentWithCurrentNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "constructorCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "ambiguousNames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "sanitizedAccessors")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

coqEnvironmentWithSanitizedAccessors :: Phantoms.TTerm Environment.CoqEnvironment -> Phantoms.TTerm (S.Set String) -> Phantoms.TTerm Environment.CoqEnvironment
coqEnvironmentWithSanitizedAccessors original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "currentNamespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "currentNamespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constructorCounts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "constructorCounts")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "ambiguousNames"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coq.environment.CoqEnvironment"),
              Core.projectionField = (Core.Name "ambiguousNames")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sanitizedAccessors"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
