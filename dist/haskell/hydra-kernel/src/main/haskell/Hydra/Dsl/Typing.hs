-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.typing

module Hydra.Dsl.Typing where
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL constructor for hydra.typing.FunctionStructure
functionStructure :: Phantoms.TTerm [Core.Name] -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm [Core.Binding] -> Phantoms.TTerm Core.Term -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm env -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructure typeParams params bindings body domains codomain environment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm environment)}]}))
-- | DSL accessor for the bindings field of hydra.typing.FunctionStructure
functionStructureBindings :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Binding]
functionStructureBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body field of hydra.typing.FunctionStructure
functionStructureBody :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm Core.Term
functionStructureBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the codomain field of hydra.typing.FunctionStructure
functionStructureCodomain :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm (Maybe Core.Type)
functionStructureCodomain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "codomain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the domains field of hydra.typing.FunctionStructure
functionStructureDomains :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Type]
functionStructureDomains x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "domains")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the environment field of hydra.typing.FunctionStructure
functionStructureEnvironment :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm env
functionStructureEnvironment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "environment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the params field of hydra.typing.FunctionStructure
functionStructureParams :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name]
functionStructureParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeParams field of hydra.typing.FunctionStructure
functionStructureTypeParams :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name]
functionStructureTypeParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the bindings field of hydra.typing.FunctionStructure
functionStructureWithBindings :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Binding] -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the body field of hydra.typing.FunctionStructure
functionStructureWithBody :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the codomain field of hydra.typing.FunctionStructure
functionStructureWithCodomain :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm (Maybe Core.Type) -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithCodomain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the domains field of hydra.typing.FunctionStructure
functionStructureWithDomains :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Type] -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithDomains original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the environment field of hydra.typing.FunctionStructure
functionStructureWithEnvironment :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm env -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithEnvironment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the params field of hydra.typing.FunctionStructure
functionStructureWithParams :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.typing.FunctionStructure
functionStructureWithTypeParams :: Phantoms.TTerm (Typing.FunctionStructure env) -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm (Typing.FunctionStructure env)
functionStructureWithTypeParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.typing.InferenceResult
inferenceResult :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm Context.Context -> Phantoms.TTerm Typing.InferenceResult
inferenceResult term type_ subst classConstraints context =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm context)}]}))
-- | DSL accessor for the classConstraints field of hydra.typing.InferenceResult
inferenceResultClassConstraints :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata)
inferenceResultClassConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "classConstraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the context field of hydra.typing.InferenceResult
inferenceResultContext :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Context.Context
inferenceResultContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "context")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the subst field of hydra.typing.InferenceResult
inferenceResultSubst :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Typing.TypeSubst
inferenceResultSubst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "subst")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the term field of hydra.typing.InferenceResult
inferenceResultTerm :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Term
inferenceResultTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.typing.InferenceResult
inferenceResultType :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Type
inferenceResultType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the classConstraints field of hydra.typing.InferenceResult
inferenceResultWithClassConstraints :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm Typing.InferenceResult
inferenceResultWithClassConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the context field of hydra.typing.InferenceResult
inferenceResultWithContext :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Context.Context -> Phantoms.TTerm Typing.InferenceResult
inferenceResultWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the subst field of hydra.typing.InferenceResult
inferenceResultWithSubst :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm Typing.InferenceResult
inferenceResultWithSubst original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the term field of hydra.typing.InferenceResult
inferenceResultWithTerm :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Typing.InferenceResult
inferenceResultWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typing.InferenceResult
inferenceResultWithType :: Phantoms.TTerm Typing.InferenceResult -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.InferenceResult
inferenceResultWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.typing.TermSubst wrapper
termSubst :: Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Typing.TermSubst
termSubst x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typing.TermSubst"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.typing.TypeClass
typeClass :: Phantoms.TTerm String -> Phantoms.TTerm Typing.TypeClass
typeClass description =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeClass"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)}]}))
-- | DSL accessor for the description field of hydra.typing.TypeClass
typeClassDescription :: Phantoms.TTerm Typing.TypeClass -> Phantoms.TTerm String
typeClassDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeClass"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the description field of hydra.typing.TypeClass
typeClassWithDescription :: Phantoms.TTerm Typing.TypeClass -> Phantoms.TTerm String -> Phantoms.TTerm Typing.TypeClass
typeClassWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeClass"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.typing.TypeConstraint
typeConstraint :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm String -> Phantoms.TTerm Typing.TypeConstraint
typeConstraint left right comment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm comment)}]}))
-- | DSL accessor for the comment field of hydra.typing.TypeConstraint
typeConstraintComment :: Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm String
typeConstraintComment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the left field of hydra.typing.TypeConstraint
typeConstraintLeft :: Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type
typeConstraintLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the right field of hydra.typing.TypeConstraint
typeConstraintRight :: Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type
typeConstraintRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the comment field of hydra.typing.TypeConstraint
typeConstraintWithComment :: Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm String -> Phantoms.TTerm Typing.TypeConstraint
typeConstraintWithComment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the left field of hydra.typing.TypeConstraint
typeConstraintWithLeft :: Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.TypeConstraint
typeConstraintWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typing.TypeConstraint
typeConstraintWithRight :: Phantoms.TTerm Typing.TypeConstraint -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Typing.TypeConstraint
typeConstraintWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.typing.TypeSubst wrapper
typeSubst :: Phantoms.TTerm (M.Map Core.Name Core.Type) -> Phantoms.TTerm Typing.TypeSubst
typeSubst x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typing.TypeSubst"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.typing.TermSubst
unTermSubst :: Phantoms.TTerm Typing.TermSubst -> Phantoms.TTerm (M.Map Core.Name Core.Term)
unTermSubst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typing.TermSubst")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.typing.TypeSubst
unTypeSubst :: Phantoms.TTerm Typing.TypeSubst -> Phantoms.TTerm (M.Map Core.Name Core.Type)
unTypeSubst x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typing.TypeSubst")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
