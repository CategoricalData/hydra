-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.typing

module Hydra.Dsl.Typing where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Paths as Paths
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL constructor for hydra.typing.FunctionStructure
functionStructure :: Typed.TypedTerm [Core.Name] -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm Core.Term -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Maybe Core.Type) -> Typed.TypedTerm env -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructure typeParams params bindings body domains codomain environment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm typeParams)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Typed.unTypedTerm domains)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Typed.unTypedTerm codomain)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Typed.unTypedTerm environment)}]}))
-- | DSL accessor for the bindings field of hydra.typing.FunctionStructure
functionStructureBindings :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Binding]
functionStructureBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.typing.FunctionStructure
functionStructureBody :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm Core.Term
functionStructureBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the codomain field of hydra.typing.FunctionStructure
functionStructureCodomain :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm (Maybe Core.Type)
functionStructureCodomain x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "codomain")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the domains field of hydra.typing.FunctionStructure
functionStructureDomains :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Type]
functionStructureDomains x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "domains")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the environment field of hydra.typing.FunctionStructure
functionStructureEnvironment :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm env
functionStructureEnvironment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "environment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.typing.FunctionStructure
functionStructureParams :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Name]
functionStructureParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParams field of hydra.typing.FunctionStructure
functionStructureTypeParams :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Name]
functionStructureTypeParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
        Core.projectionFieldName = (Core.Name "typeParams")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the bindings field of hydra.typing.FunctionStructure
functionStructureWithBindings :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Binding] -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.typing.FunctionStructure
functionStructureWithBody :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the codomain field of hydra.typing.FunctionStructure
functionStructureWithCodomain :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm (Maybe Core.Type) -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithCodomain original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the domains field of hydra.typing.FunctionStructure
functionStructureWithDomains :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Type] -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithDomains original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the environment field of hydra.typing.FunctionStructure
functionStructureWithEnvironment :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm env -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithEnvironment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the params field of hydra.typing.FunctionStructure
functionStructureWithParams :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "typeParams")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeParams field of hydra.typing.FunctionStructure
functionStructureWithTypeParams :: Typed.TypedTerm (Typing.FunctionStructure env) -> Typed.TypedTerm [Core.Name] -> Typed.TypedTerm (Typing.FunctionStructure env)
functionStructureWithTypeParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.FunctionStructure"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParams"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "domains"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "domains")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "codomain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "codomain")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.FunctionStructure"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typing.InferenceContext
inferenceContext :: Typed.TypedTerm Int -> Typed.TypedTerm [Paths.SubtermStep] -> Typed.TypedTerm Typing.InferenceContext
inferenceContext freshTypeVariableCount trace =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "freshTypeVariableCount"),
          Core.fieldTerm = (Typed.unTypedTerm freshTypeVariableCount)},
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Typed.unTypedTerm trace)}]}))
-- | DSL accessor for the freshTypeVariableCount field of hydra.typing.InferenceContext
inferenceContextFreshTypeVariableCount :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Int
inferenceContextFreshTypeVariableCount x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceContext"),
        Core.projectionFieldName = (Core.Name "freshTypeVariableCount")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the trace field of hydra.typing.InferenceContext
inferenceContextTrace :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm [Paths.SubtermStep]
inferenceContextTrace x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceContext"),
        Core.projectionFieldName = (Core.Name "trace")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the freshTypeVariableCount field of hydra.typing.InferenceContext
inferenceContextWithFreshTypeVariableCount :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Int -> Typed.TypedTerm Typing.InferenceContext
inferenceContextWithFreshTypeVariableCount original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "freshTypeVariableCount"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceContext"),
              Core.projectionFieldName = (Core.Name "trace")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the trace field of hydra.typing.InferenceContext
inferenceContextWithTrace :: Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm [Paths.SubtermStep] -> Typed.TypedTerm Typing.InferenceContext
inferenceContextWithTrace original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "freshTypeVariableCount"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceContext"),
              Core.projectionFieldName = (Core.Name "freshTypeVariableCount")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typing.InferenceResult
inferenceResult :: Typed.TypedTerm Core.Term -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.TypeSubst -> Typed.TypedTerm (M.Map Core.Name Core.TypeVariableConstraints) -> Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Typing.InferenceResult
inferenceResult term type_ subst classConstraints context =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Typed.unTypedTerm subst)},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Typed.unTypedTerm classConstraints)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Typed.unTypedTerm context)}]}))
-- | DSL accessor for the classConstraints field of hydra.typing.InferenceResult
inferenceResultClassConstraints :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm (M.Map Core.Name Core.TypeVariableConstraints)
inferenceResultClassConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "classConstraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the context field of hydra.typing.InferenceResult
inferenceResultContext :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Typing.InferenceContext
inferenceResultContext x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "context")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the subst field of hydra.typing.InferenceResult
inferenceResultSubst :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Typing.TypeSubst
inferenceResultSubst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "subst")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.typing.InferenceResult
inferenceResultTerm :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Core.Term
inferenceResultTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typing.InferenceResult
inferenceResultType :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Core.Type
inferenceResultType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the classConstraints field of hydra.typing.InferenceResult
inferenceResultWithClassConstraints :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm (M.Map Core.Name Core.TypeVariableConstraints) -> Typed.TypedTerm Typing.InferenceResult
inferenceResultWithClassConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the context field of hydra.typing.InferenceResult
inferenceResultWithContext :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Typing.InferenceContext -> Typed.TypedTerm Typing.InferenceResult
inferenceResultWithContext original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the subst field of hydra.typing.InferenceResult
inferenceResultWithSubst :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Typing.TypeSubst -> Typed.TypedTerm Typing.InferenceResult
inferenceResultWithSubst original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.typing.InferenceResult
inferenceResultWithTerm :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Typing.InferenceResult
inferenceResultWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typing.InferenceResult
inferenceResultWithType :: Typed.TypedTerm Typing.InferenceResult -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.InferenceResult
inferenceResultWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.InferenceResult"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subst"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "subst")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.InferenceResult"),
              Core.projectionFieldName = (Core.Name "context")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typing.Parameter
parameter :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Bool -> Typed.TypedTerm Typing.Parameter
parameter name description type_ isLazy =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "isLazy"),
          Core.fieldTerm = (Typed.unTypedTerm isLazy)}]}))
-- | DSL accessor for the description field of hydra.typing.Parameter
parameterDescription :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm (Maybe String)
parameterDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the isLazy field of hydra.typing.Parameter
parameterIsLazy :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm Bool
parameterIsLazy x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
        Core.projectionFieldName = (Core.Name "isLazy")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.typing.Parameter
parameterName :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm Core.Name
parameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typing.Parameter
parameterType :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm Core.Type
parameterType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the description field of hydra.typing.Parameter
parameterWithDescription :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Typing.Parameter
parameterWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLazy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "isLazy")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the isLazy field of hydra.typing.Parameter
parameterWithIsLazy :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm Bool -> Typed.TypedTerm Typing.Parameter
parameterWithIsLazy original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLazy"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.typing.Parameter
parameterWithName :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Typing.Parameter
parameterWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isLazy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "isLazy")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typing.Parameter
parameterWithType :: Typed.TypedTerm Typing.Parameter -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.Parameter
parameterWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isLazy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Parameter"),
              Core.projectionFieldName = (Core.Name "isLazy")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typing.Result
result :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.Result
result description type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Result"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the description field of hydra.typing.Result
resultDescription :: Typed.TypedTerm Typing.Result -> Typed.TypedTerm (Maybe String)
resultDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.Result"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typing.Result
resultType :: Typed.TypedTerm Typing.Result -> Typed.TypedTerm Core.Type
resultType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.Result"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the description field of hydra.typing.Result
resultWithDescription :: Typed.TypedTerm Typing.Result -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Typing.Result
resultWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Result"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Result"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typing.Result
resultWithType :: Typed.TypedTerm Typing.Result -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.Result
resultWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.Result"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.Result"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typing.TermSignature
termSignature :: Typed.TypedTerm [Typing.TypeParameter] -> Typed.TypedTerm [Typing.Parameter] -> Typed.TypedTerm Typing.Result -> Typed.TypedTerm Typing.TermSignature
termSignature typeParameters parameters result =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TermSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm typeParameters)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Typed.unTypedTerm result)}]}))
-- | DSL accessor for the parameters field of hydra.typing.TermSignature
termSignatureParameters :: Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm [Typing.Parameter]
termSignatureParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the result field of hydra.typing.TermSignature
termSignatureResult :: Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm Typing.Result
termSignatureResult x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
        Core.projectionFieldName = (Core.Name "result")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParameters field of hydra.typing.TermSignature
termSignatureTypeParameters :: Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm [Typing.TypeParameter]
termSignatureTypeParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
        Core.projectionFieldName = (Core.Name "typeParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the parameters field of hydra.typing.TermSignature
termSignatureWithParameters :: Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm [Typing.Parameter] -> Typed.TypedTerm Typing.TermSignature
termSignatureWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TermSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the result field of hydra.typing.TermSignature
termSignatureWithResult :: Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm Typing.Result -> Typed.TypedTerm Typing.TermSignature
termSignatureWithResult original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TermSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeParameters field of hydra.typing.TermSignature
termSignatureWithTypeParameters :: Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm [Typing.TypeParameter] -> Typed.TypedTerm Typing.TermSignature
termSignatureWithTypeParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TermSignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TermSignature"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typing.TermSubst wrapper
termSubst :: Typed.TypedTerm (M.Map Core.Name Core.Term) -> Typed.TypedTerm Typing.TermSubst
termSubst x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typing.TermSubst"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.typing.TypeClass
typeClass :: Typed.TypedTerm String -> Typed.TypedTerm Typing.TypeClass
typeClass description =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeClass"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)}]}))
-- | DSL accessor for the description field of hydra.typing.TypeClass
typeClassDescription :: Typed.TypedTerm Typing.TypeClass -> Typed.TypedTerm String
typeClassDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeClass"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the description field of hydra.typing.TypeClass
typeClassWithDescription :: Typed.TypedTerm Typing.TypeClass -> Typed.TypedTerm String -> Typed.TypedTerm Typing.TypeClass
typeClassWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeClass"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typing.TypeConstraint
typeConstraint :: Typed.TypedTerm Core.Type -> Typed.TypedTerm Core.Type -> Typed.TypedTerm String -> Typed.TypedTerm Typing.TypeConstraint
typeConstraint left right comment =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm comment)}]}))
-- | DSL accessor for the comment field of hydra.typing.TypeConstraint
typeConstraintComment :: Typed.TypedTerm Typing.TypeConstraint -> Typed.TypedTerm String
typeConstraintComment x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
        Core.projectionFieldName = (Core.Name "comment")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the left field of hydra.typing.TypeConstraint
typeConstraintLeft :: Typed.TypedTerm Typing.TypeConstraint -> Typed.TypedTerm Core.Type
typeConstraintLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.typing.TypeConstraint
typeConstraintRight :: Typed.TypedTerm Typing.TypeConstraint -> Typed.TypedTerm Core.Type
typeConstraintRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comment field of hydra.typing.TypeConstraint
typeConstraintWithComment :: Typed.TypedTerm Typing.TypeConstraint -> Typed.TypedTerm String -> Typed.TypedTerm Typing.TypeConstraint
typeConstraintWithComment original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the left field of hydra.typing.TypeConstraint
typeConstraintWithLeft :: Typed.TypedTerm Typing.TypeConstraint -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.TypeConstraint
typeConstraintWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typing.TypeConstraint
typeConstraintWithRight :: Typed.TypedTerm Typing.TypeConstraint -> Typed.TypedTerm Core.Type -> Typed.TypedTerm Typing.TypeConstraint
typeConstraintWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeConstraint"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeConstraint"),
              Core.projectionFieldName = (Core.Name "comment")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typing.TypeParameter
typeParameter :: Typed.TypedTerm Core.Name -> Typed.TypedTerm [Core.TypeClassConstraint] -> Typed.TypedTerm Typing.TypeParameter
typeParameter name constraints =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm constraints)}]}))
-- | DSL accessor for the constraints field of hydra.typing.TypeParameter
typeParameterConstraints :: Typed.TypedTerm Typing.TypeParameter -> Typed.TypedTerm [Core.TypeClassConstraint]
typeParameterConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeParameter"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.typing.TypeParameter
typeParameterName :: Typed.TypedTerm Typing.TypeParameter -> Typed.TypedTerm Core.Name
typeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typing.TypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the constraints field of hydra.typing.TypeParameter
typeParameterWithConstraints :: Typed.TypedTerm Typing.TypeParameter -> Typed.TypedTerm [Core.TypeClassConstraint] -> Typed.TypedTerm Typing.TypeParameter
typeParameterWithConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.typing.TypeParameter
typeParameterWithName :: Typed.TypedTerm Typing.TypeParameter -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Typing.TypeParameter
typeParameterWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typing.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typing.TypeParameter"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typing.TypeSubst wrapper
typeSubst :: Typed.TypedTerm (M.Map Core.Name Core.Type) -> Typed.TypedTerm Typing.TypeSubst
typeSubst x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typing.TypeSubst"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typing.TermSubst
unTermSubst :: Typed.TypedTerm Typing.TermSubst -> Typed.TypedTerm (M.Map Core.Name Core.Term)
unTermSubst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typing.TermSubst")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typing.TypeSubst
unTypeSubst :: Typed.TypedTerm Typing.TypeSubst -> Typed.TypedTerm (M.Map Core.Name Core.Type)
unTypeSubst x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typing.TypeSubst")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
