-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.python.environment

module Hydra.Dsl.Python.Environment where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Python.Syntax as PythonSyntax
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Graph as Graph
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Python.Environment as Environment
import qualified Hydra.Python.Syntax as Syntax
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.python.environment.PyGraph
pyGraph :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Environment.PyGraph
pyGraph graph metadata =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)}]}))
-- | DSL accessor for the graph field of hydra.python.environment.PyGraph
pyGraphGraph :: Typed.TypedTerm Environment.PyGraph -> Typed.TypedTerm Graph.Graph
pyGraphGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PyGraph"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the metadata field of hydra.python.environment.PyGraph
pyGraphMetadata :: Typed.TypedTerm Environment.PyGraph -> Typed.TypedTerm Environment.PythonModuleMetadata
pyGraphMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PyGraph"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.python.environment.PyGraph
pyGraphWithGraph :: Typed.TypedTerm Environment.PyGraph -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Environment.PyGraph
pyGraphWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PyGraph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the metadata field of hydra.python.environment.PyGraph
pyGraphWithMetadata :: Typed.TypedTerm Environment.PyGraph -> Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Environment.PyGraph
pyGraphWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PyGraph"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.python.environment.PythonEnvironment
pythonEnvironment :: Typed.TypedTerm (Util.ModuleNames Syntax.DottedName) -> Typed.TypedTerm ([Core.Name], (M.Map Core.Name Syntax.Name)) -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.PythonVersion -> Typed.TypedTerm Bool -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironment namespaces boundTypeVariables graph nullaryBindings version skipCasts inlineVariables =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Typed.unTypedTerm namespaces)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Typed.unTypedTerm boundTypeVariables)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Typed.unTypedTerm nullaryBindings)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Typed.unTypedTerm version)},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Typed.unTypedTerm skipCasts)},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Typed.unTypedTerm inlineVariables)}]}))
-- | DSL accessor for the boundTypeVariables field of hydra.python.environment.PythonEnvironment
pythonEnvironmentBoundTypeVariables :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm ([Core.Name], (M.Map Core.Name Syntax.Name))
pythonEnvironmentBoundTypeVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the graph field of hydra.python.environment.PythonEnvironment
pythonEnvironmentGraph :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Graph.Graph
pythonEnvironmentGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inlineVariables field of hydra.python.environment.PythonEnvironment
pythonEnvironmentInlineVariables :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm (S.Set Core.Name)
pythonEnvironmentInlineVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "inlineVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the namespaces field of hydra.python.environment.PythonEnvironment
pythonEnvironmentNamespaces :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm (Util.ModuleNames Syntax.DottedName)
pythonEnvironmentNamespaces x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "namespaces")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the nullaryBindings field of hydra.python.environment.PythonEnvironment
pythonEnvironmentNullaryBindings :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm (S.Set Core.Name)
pythonEnvironmentNullaryBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "nullaryBindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the skipCasts field of hydra.python.environment.PythonEnvironment
pythonEnvironmentSkipCasts :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Bool
pythonEnvironmentSkipCasts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "skipCasts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the version field of hydra.python.environment.PythonEnvironment
pythonEnvironmentVersion :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Environment.PythonVersion
pythonEnvironmentVersion x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "version")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the boundTypeVariables field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithBoundTypeVariables :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm ([Core.Name], (M.Map Core.Name Syntax.Name)) -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithBoundTypeVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "nullaryBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "skipCasts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "inlineVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the graph field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithGraph :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "nullaryBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "skipCasts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "inlineVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the inlineVariables field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithInlineVariables :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithInlineVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "nullaryBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "skipCasts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the namespaces field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithNamespaces :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm (Util.ModuleNames Syntax.DottedName) -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithNamespaces original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "nullaryBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "skipCasts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "inlineVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the nullaryBindings field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithNullaryBindings :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithNullaryBindings original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "skipCasts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "inlineVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the skipCasts field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithSkipCasts :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithSkipCasts original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "nullaryBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "inlineVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the version field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithVersion :: Typed.TypedTerm Environment.PythonEnvironment -> Typed.TypedTerm Environment.PythonVersion -> Typed.TypedTerm Environment.PythonEnvironment
pythonEnvironmentWithVersion original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "nullaryBindings")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "skipCasts")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
              Core.projectionFieldName = (Core.Name "inlineVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.python.environment.PythonModuleMetadata
pythonModuleMetadata :: Typed.TypedTerm (Util.ModuleNames Syntax.DottedName) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadata namespaces typeVariables usesAnnotated usesCallable usesCast usesLruCache usesTypeAlias usesDataclass usesDecimal usesEither usesEnum usesFrozenDict usesFrozenList usesFrozenSet usesGeneric usesJust usesLeft usesMaybe usesName usesNode usesNothing usesRight usesTypeVar =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Typed.unTypedTerm namespaces)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Typed.unTypedTerm typeVariables)},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Typed.unTypedTerm usesAnnotated)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Typed.unTypedTerm usesCallable)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Typed.unTypedTerm usesCast)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Typed.unTypedTerm usesLruCache)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Typed.unTypedTerm usesTypeAlias)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Typed.unTypedTerm usesDataclass)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Typed.unTypedTerm usesDecimal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Typed.unTypedTerm usesEither)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Typed.unTypedTerm usesEnum)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Typed.unTypedTerm usesFrozenDict)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Typed.unTypedTerm usesFrozenList)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Typed.unTypedTerm usesFrozenSet)},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Typed.unTypedTerm usesGeneric)},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Typed.unTypedTerm usesJust)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Typed.unTypedTerm usesLeft)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Typed.unTypedTerm usesMaybe)},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Typed.unTypedTerm usesName)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Typed.unTypedTerm usesNode)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Typed.unTypedTerm usesNothing)},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Typed.unTypedTerm usesRight)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Typed.unTypedTerm usesTypeVar)}]}))
-- | DSL accessor for the namespaces field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataNamespaces :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm (Util.ModuleNames Syntax.DottedName)
pythonModuleMetadataNamespaces x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "namespaces")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVariables field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataTypeVariables :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm (S.Set Core.Name)
pythonModuleMetadataTypeVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "typeVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesAnnotated field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesAnnotated :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesAnnotated x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesAnnotated")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesCallable field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesCallable :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesCallable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesCallable")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesCast field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesCast :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesCast x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesCast")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesDataclass field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesDataclass :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesDataclass x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesDataclass")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesDecimal field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesDecimal :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesDecimal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesDecimal")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesEither field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesEither :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesEither x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesEither")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesEnum field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesEnum :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesEnum x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesEnum")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesFrozenDict field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesFrozenDict :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesFrozenDict x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesFrozenList field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesFrozenList :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesFrozenList x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesFrozenList")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesFrozenSet field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesFrozenSet :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesFrozenSet x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesGeneric field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesGeneric :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesGeneric x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesGeneric")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesJust field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesJust :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesJust x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesJust")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesLeft field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesLeft :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesLeft")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesLruCache field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesLruCache :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesLruCache x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesLruCache")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesMaybe field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesMaybe :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesMaybe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesMaybe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesName field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesName :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesNode field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesNode :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesNode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesNode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesNothing field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesNothing :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesNothing x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesNothing")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesRight field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesRight :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesRight")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesTypeAlias field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesTypeAlias :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesTypeAlias x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesTypeVar field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesTypeVar :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool
pythonModuleMetadataUsesTypeVar x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesTypeVar")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the namespaces field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithNamespaces :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm (Util.ModuleNames Syntax.DottedName) -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithNamespaces original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeVariables field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithTypeVariables :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithTypeVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesAnnotated field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesAnnotated :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesAnnotated original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesCallable field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesCallable :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesCallable original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesCast field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesCast :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesCast original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesDataclass field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesDataclass :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesDataclass original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesDecimal field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesDecimal :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesDecimal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesEither field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesEither :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesEither original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesEnum field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesEnum :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesEnum original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesFrozenDict field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenDict :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenDict original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesFrozenList field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenList :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenList original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesFrozenSet field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenSet :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenSet original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesGeneric field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesGeneric :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesGeneric original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesJust field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesJust :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesJust original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesLeft field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesLeft :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesLruCache field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesLruCache :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesLruCache original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesMaybe field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesMaybe :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesMaybe original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesName field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesName :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesNode field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesNode :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesNode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesNothing field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesNothing :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesNothing original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesRight field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesRight :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesTypeAlias field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesTypeAlias :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesTypeAlias original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeVar")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the usesTypeVar field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithUsesTypeVar :: Typed.TypedTerm Environment.PythonModuleMetadata -> Typed.TypedTerm Bool -> Typed.TypedTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesTypeVar original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "namespaces")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesAnnotated")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCallable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesCast")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLruCache")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDataclass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesDecimal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEither")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesEnum")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenList")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenSet"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesGeneric")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesJust")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesLeft")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesMaybe")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesNothing")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
              Core.projectionFieldName = (Core.Name "usesRight")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the python310 variant of hydra.python.environment.PythonVersion
pythonVersionPython310 :: Typed.TypedTerm Environment.PythonVersion
pythonVersionPython310 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.environment.PythonVersion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "python310"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the python312 variant of hydra.python.environment.PythonVersion
pythonVersionPython312 :: Typed.TypedTerm Environment.PythonVersion
pythonVersionPython312 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.environment.PythonVersion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "python312"),
        Core.fieldTerm = Core.TermUnit}}))
