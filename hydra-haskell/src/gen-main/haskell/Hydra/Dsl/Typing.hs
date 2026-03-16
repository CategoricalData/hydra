-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.typing

module Hydra.Dsl.Typing where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

functionStructure :: (Phantoms.TTerm [Core.Name] -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm [Core.Binding] -> Phantoms.TTerm Core.Term -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm env -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructure typeParams params bindings body domains codomain environment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm typeParams)},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm params)},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Phantoms.unTTerm bindings)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm body)},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Phantoms.unTTerm domains)},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Phantoms.unTTerm codomain)},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Phantoms.unTTerm environment)}]})))

functionStructureTypeParams :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name])
functionStructureTypeParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "typeParams")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureParams :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name])
functionStructureParams x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "params")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureBindings :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Binding])
functionStructureBindings x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "bindings")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureBody :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm Core.Term)
functionStructureBody x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "body")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureDomains :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Type])
functionStructureDomains x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "domains")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureCodomain :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm (Maybe Core.Type))
functionStructureCodomain x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "codomain")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureEnvironment :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm env)
functionStructureEnvironment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
    Core.projectionField = (Core.Name "environment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

functionStructureWithTypeParams :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithTypeParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "bindings")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "domains")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "codomain")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "environment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionStructureWithParams :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithParams original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "bindings")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "domains")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "codomain")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "environment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionStructureWithBindings :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Binding] -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithBindings original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "domains")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "codomain")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "environment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionStructureWithBody :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithBody original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "bindings")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "domains")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "codomain")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "environment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionStructureWithDomains :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithDomains original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "bindings")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "codomain")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "environment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionStructureWithCodomain :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithCodomain original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "bindings")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "domains")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "environment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

functionStructureWithEnvironment :: (Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm env -> Phantoms.TTerm (Typing.FunctionStructure env))
functionStructureWithEnvironment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "typeParams"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "typeParams")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "params"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "params")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "bindings"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "bindings")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "body"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "body")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "domains"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "domains")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "codomain"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
          Core.projectionField = (Core.Name "codomain")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "environment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

inferenceResult :: (Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm Context.Context -> Phantoms.TTerm Typing.InferenceResult)
inferenceResult term type_ subst classConstraints context = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Phantoms.unTTerm term)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (Phantoms.unTTerm subst)},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Phantoms.unTTerm classConstraints)},
    Core.Field {
      Core.fieldName = (Core.Name "context"),
      Core.fieldTerm = (Phantoms.unTTerm context)}]})))

inferenceResultTerm :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Term)
inferenceResultTerm x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
    Core.projectionField = (Core.Name "term")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

inferenceResultType :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Type)
inferenceResultType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

inferenceResultSubst :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Typing.TypeSubst)
inferenceResultSubst x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
    Core.projectionField = (Core.Name "subst")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

inferenceResultClassConstraints :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata))
inferenceResultClassConstraints x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
    Core.projectionField = (Core.Name "classConstraints")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

inferenceResultContext :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Context.Context)
inferenceResultContext x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
    Core.projectionField = (Core.Name "context")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

inferenceResultWithTerm :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Typing.InferenceResult)
inferenceResultWithTerm original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "subst")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "context"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "context")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

inferenceResultWithType :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.InferenceResult)
inferenceResultWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "term")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "subst")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "context"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "context")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

inferenceResultWithSubst :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Typing.InferenceResult)
inferenceResultWithSubst original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "term")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "context"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "context")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

inferenceResultWithClassConstraints :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm Typing.InferenceResult)
inferenceResultWithClassConstraints original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "term")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "subst")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "context"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "context")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

inferenceResultWithContext :: (Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Context.Context -> Phantoms.TTerm Typing.InferenceResult)
inferenceResultWithContext original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "term"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "term")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "subst"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "subst")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "context"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

termSubst :: (Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Typing.TermSubst)
termSubst x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.typing.TermSubst"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTermSubst :: (Phantoms.TTerm Typing.TermSubst -> Phantoms.TTerm (M.Map Core.Name Core.Term))
unTermSubst x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.typing.TermSubst")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeConstraint :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm String -> Phantoms.TTerm Typing.TypeConstraint)
typeConstraint left right comment = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm left)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm right)},
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Phantoms.unTTerm comment)}]})))

typeConstraintLeft :: (Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type)
typeConstraintLeft x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
    Core.projectionField = (Core.Name "left")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeConstraintRight :: (Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type)
typeConstraintRight x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
    Core.projectionField = (Core.Name "right")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeConstraintComment :: (Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm String)
typeConstraintComment x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
    Core.projectionField = (Core.Name "comment")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

typeConstraintWithLeft :: (Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.TypeConstraint)
typeConstraintWithLeft original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
          Core.projectionField = (Core.Name "comment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeConstraintWithRight :: (Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.TypeConstraint)
typeConstraintWithRight original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
          Core.projectionField = (Core.Name "comment")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

typeConstraintWithComment :: (Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm String -> Phantoms.TTerm Typing.TypeConstraint)
typeConstraintWithComment original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "comment"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

typeSubst :: (Phantoms.TTerm (M.Map Core.Name Core.Type) -> Phantoms.TTerm Typing.TypeSubst)
typeSubst x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.typing.TypeSubst"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unTypeSubst :: (Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm (M.Map Core.Name Core.Type))
unTypeSubst x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.typing.TypeSubst")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))
