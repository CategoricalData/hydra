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
-- | DSL constructor for hydra.python.environment.PyGraph
pyGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pyGraphGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pyGraphGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PyGraph"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the metadata field of hydra.python.environment.PyGraph
pyGraphMetadata :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pyGraphMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PyGraph"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the graph field of hydra.python.environment.PyGraph
pyGraphWithGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pyGraphWithMetadata :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironment :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6 -> Typed.TypedTerm t7
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
pythonEnvironmentBoundTypeVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentBoundTypeVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "boundTypeVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the graph field of hydra.python.environment.PythonEnvironment
pythonEnvironmentGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the inlineVariables field of hydra.python.environment.PythonEnvironment
pythonEnvironmentInlineVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentInlineVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "inlineVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the namespaces field of hydra.python.environment.PythonEnvironment
pythonEnvironmentNamespaces :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentNamespaces x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "namespaces")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the nullaryBindings field of hydra.python.environment.PythonEnvironment
pythonEnvironmentNullaryBindings :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentNullaryBindings x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "nullaryBindings")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the skipCasts field of hydra.python.environment.PythonEnvironment
pythonEnvironmentSkipCasts :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentSkipCasts x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "skipCasts")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the version field of hydra.python.environment.PythonEnvironment
pythonEnvironmentVersion :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonEnvironmentVersion x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonEnvironment"),
        Core.projectionFieldName = (Core.Name "version")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the boundTypeVariables field of hydra.python.environment.PythonEnvironment
pythonEnvironmentWithBoundTypeVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironmentWithGraph :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironmentWithInlineVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironmentWithNamespaces :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironmentWithNullaryBindings :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironmentWithSkipCasts :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonEnvironmentWithVersion :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadata :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm t3 -> Typed.TypedTerm t4 -> Typed.TypedTerm t5 -> Typed.TypedTerm t6 -> Typed.TypedTerm t7 -> Typed.TypedTerm t8 -> Typed.TypedTerm t9 -> Typed.TypedTerm t10 -> Typed.TypedTerm t11 -> Typed.TypedTerm t12 -> Typed.TypedTerm t13 -> Typed.TypedTerm t14 -> Typed.TypedTerm t15 -> Typed.TypedTerm t16 -> Typed.TypedTerm t17 -> Typed.TypedTerm t18 -> Typed.TypedTerm t19 -> Typed.TypedTerm t20 -> Typed.TypedTerm t21 -> Typed.TypedTerm t22 -> Typed.TypedTerm t23
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
pythonModuleMetadataNamespaces :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataNamespaces x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "namespaces")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVariables field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataTypeVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataTypeVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "typeVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesAnnotated field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesAnnotated :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesAnnotated x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesAnnotated")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesCallable field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesCallable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesCallable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesCallable")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesCast field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesCast :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesCast x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesCast")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesDataclass field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesDataclass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesDataclass x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesDataclass")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesDecimal field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesDecimal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesDecimal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesDecimal")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesEither field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesEither :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesEither x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesEither")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesEnum field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesEnum :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesEnum x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesEnum")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesFrozenDict field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesFrozenDict :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesFrozenDict x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesFrozenDict")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesFrozenList field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesFrozenList :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesFrozenList x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesFrozenList")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesFrozenSet field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesFrozenSet :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesFrozenSet x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesFrozenSet")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesGeneric field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesGeneric :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesGeneric x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesGeneric")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesJust field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesJust :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesJust x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesJust")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesLeft field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesLeft :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesLeft")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesLruCache field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesLruCache :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesLruCache x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesLruCache")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesMaybe field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesMaybe :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesMaybe x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesMaybe")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesName field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesNode field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesNode :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesNode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesNode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesNothing field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesNothing :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesNothing x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesNothing")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesRight field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesRight :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesRight")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesTypeAlias field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesTypeAlias :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesTypeAlias x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesTypeAlias")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the usesTypeVar field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataUsesTypeVar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1
pythonModuleMetadataUsesTypeVar x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.python.environment.PythonModuleMetadata"),
        Core.projectionFieldName = (Core.Name "usesTypeVar")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the namespaces field of hydra.python.environment.PythonModuleMetadata
pythonModuleMetadataWithNamespaces :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithTypeVariables :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesAnnotated :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesCallable :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesCast :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesDataclass :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesDecimal :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesEither :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesEnum :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesFrozenDict :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesFrozenList :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesFrozenSet :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesGeneric :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesJust :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesLeft :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesLruCache :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesMaybe :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesName :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesNode :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesNothing :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesRight :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesTypeAlias :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonModuleMetadataWithUsesTypeVar :: Typed.TypedTerm t0 -> Typed.TypedTerm t1 -> Typed.TypedTerm t2
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
pythonVersionPython310 :: Typed.TypedTerm t0
pythonVersionPython310 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.environment.PythonVersion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "python310"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the python312 variant of hydra.python.environment.PythonVersion
pythonVersionPython312 :: Typed.TypedTerm t0
pythonVersionPython312 =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.python.environment.PythonVersion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "python312"),
        Core.fieldTerm = Core.TermUnit}}))
