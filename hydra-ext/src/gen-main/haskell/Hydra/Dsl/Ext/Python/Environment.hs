-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.python.environment

module Hydra.Dsl.Ext.Python.Environment where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Python.Environment as Environment
import qualified Hydra.Ext.Python.Syntax as Syntax
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

pyGraph :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Environment.PyGraph
pyGraph graph metadata =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Phantoms.unTTerm metadata)}]}))

pyGraphGraph :: Phantoms.TTerm Environment.PyGraph -> Phantoms.TTerm Graph.Graph
pyGraphGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
        Core.projectionField = (Core.Name "graph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pyGraphMetadata :: Phantoms.TTerm Environment.PyGraph -> Phantoms.TTerm Environment.PythonModuleMetadata
pyGraphMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
        Core.projectionField = (Core.Name "metadata")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pyGraphWithGraph :: Phantoms.TTerm Environment.PyGraph -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.PyGraph
pyGraphWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
              Core.projectionField = (Core.Name "metadata")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pyGraphWithMetadata :: Phantoms.TTerm Environment.PyGraph -> Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Environment.PyGraph
pyGraphWithMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PyGraph"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pythonEnvironment :: Phantoms.TTerm (Module.Namespaces Syntax.DottedName) -> Phantoms.TTerm ([Core.Name], (M.Map Core.Name Syntax.Name)) -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.PythonVersion -> Phantoms.TTerm Bool -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironment namespaces boundTypeVariables graph nullaryBindings version skipCasts inlineVariables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Phantoms.unTTerm namespaces)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm boundTypeVariables)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Phantoms.unTTerm nullaryBindings)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm version)},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Phantoms.unTTerm skipCasts)},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Phantoms.unTTerm inlineVariables)}]}))

pythonEnvironmentBoundTypeVariables :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm ([Core.Name], (M.Map Core.Name Syntax.Name))
pythonEnvironmentBoundTypeVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "boundTypeVariables")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentGraph :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm Graph.Graph
pythonEnvironmentGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "graph")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentInlineVariables :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm (S.Set Core.Name)
pythonEnvironmentInlineVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "inlineVariables")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentNamespaces :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm (Module.Namespaces Syntax.DottedName)
pythonEnvironmentNamespaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "namespaces")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentNullaryBindings :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm (S.Set Core.Name)
pythonEnvironmentNullaryBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "nullaryBindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentSkipCasts :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm Bool
pythonEnvironmentSkipCasts x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "skipCasts")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentVersion :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm Environment.PythonVersion
pythonEnvironmentVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
        Core.projectionField = (Core.Name "version")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonEnvironmentWithBoundTypeVariables :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm ([Core.Name], (M.Map Core.Name Syntax.Name)) -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithBoundTypeVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "nullaryBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "skipCasts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "inlineVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonEnvironmentWithGraph :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "nullaryBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "skipCasts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "inlineVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonEnvironmentWithInlineVariables :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithInlineVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "nullaryBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "skipCasts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pythonEnvironmentWithNamespaces :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm (Module.Namespaces Syntax.DottedName) -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithNamespaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "nullaryBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "skipCasts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "inlineVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonEnvironmentWithNullaryBindings :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithNullaryBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "skipCasts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "inlineVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonEnvironmentWithSkipCasts :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithSkipCasts original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "nullaryBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "version")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "inlineVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonEnvironmentWithVersion :: Phantoms.TTerm Environment.PythonEnvironment -> Phantoms.TTerm Environment.PythonVersion -> Phantoms.TTerm Environment.PythonEnvironment
pythonEnvironmentWithVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "boundTypeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "graph")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "nullaryBindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "nullaryBindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "skipCasts"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "skipCasts")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "inlineVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonEnvironment"),
              Core.projectionField = (Core.Name "inlineVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadata :: Phantoms.TTerm (Module.Namespaces Syntax.DottedName) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadata namespaces typeVariables usesAnnotated usesCallable usesCast usesLruCache usesTypeAlias usesDataclass usesDecimal usesEither usesEnum usesFrozenDict usesFrozenList usesGeneric usesJust usesLeft usesMaybe usesName usesNode usesNothing usesRight usesTypeVar =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Phantoms.unTTerm namespaces)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm typeVariables)},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Phantoms.unTTerm usesAnnotated)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Phantoms.unTTerm usesCallable)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Phantoms.unTTerm usesCast)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Phantoms.unTTerm usesLruCache)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Phantoms.unTTerm usesTypeAlias)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Phantoms.unTTerm usesDataclass)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Phantoms.unTTerm usesDecimal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Phantoms.unTTerm usesEither)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Phantoms.unTTerm usesEnum)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Phantoms.unTTerm usesFrozenDict)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Phantoms.unTTerm usesFrozenList)},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Phantoms.unTTerm usesGeneric)},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Phantoms.unTTerm usesJust)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Phantoms.unTTerm usesLeft)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Phantoms.unTTerm usesMaybe)},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Phantoms.unTTerm usesName)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Phantoms.unTTerm usesNode)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Phantoms.unTTerm usesNothing)},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Phantoms.unTTerm usesRight)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Phantoms.unTTerm usesTypeVar)}]}))

pythonModuleMetadataNamespaces :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm (Module.Namespaces Syntax.DottedName)
pythonModuleMetadataNamespaces x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "namespaces")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataTypeVariables :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm (S.Set Core.Name)
pythonModuleMetadataTypeVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "typeVariables")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesAnnotated :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesAnnotated x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesAnnotated")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesCallable :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesCallable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesCallable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesCast :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesCast x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesCast")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesDataclass :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesDataclass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesDataclass")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesDecimal :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesDecimal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesDecimal")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesEither :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesEither x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesEither")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesEnum :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesEnum x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesEnum")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesFrozenDict :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesFrozenDict x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesFrozenDict")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesFrozenList :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesFrozenList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesFrozenList")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesGeneric :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesGeneric x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesGeneric")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesJust :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesJust x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesJust")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesLeft :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesLeft")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesLruCache :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesLruCache x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesLruCache")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesMaybe :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesMaybe x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesMaybe")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesName :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesNode :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesNode")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesNothing :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesNothing x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesNothing")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesRight :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesRight")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesTypeAlias :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesTypeAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesTypeAlias")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataUsesTypeVar :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool
pythonModuleMetadataUsesTypeVar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
        Core.projectionField = (Core.Name "usesTypeVar")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pythonModuleMetadataWithNamespaces :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm (Module.Namespaces Syntax.DottedName) -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithNamespaces original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithTypeVariables :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithTypeVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesAnnotated :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesAnnotated original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesCallable :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesCallable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesCast :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesCast original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesDataclass :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesDataclass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesDecimal :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesDecimal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesEither :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesEither original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesEnum :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesEnum original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesFrozenDict :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenDict original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesFrozenList :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesFrozenList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesGeneric :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesGeneric original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesJust :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesJust original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesLeft :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesLruCache :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesLruCache original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesMaybe :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesMaybe original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesName :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesNode :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesNode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesNothing :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesNothing original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesRight :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesTypeAlias :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesTypeAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeVar")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pythonModuleMetadataWithUsesTypeVar :: Phantoms.TTerm Environment.PythonModuleMetadata -> Phantoms.TTerm Bool -> Phantoms.TTerm Environment.PythonModuleMetadata
pythonModuleMetadataWithUsesTypeVar original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespaces"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "namespaces")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "typeVariables")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesAnnotated"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesAnnotated")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCallable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCallable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesCast"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesCast")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLruCache"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLruCache")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeAlias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesTypeAlias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDataclass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDataclass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesDecimal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesDecimal")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEither"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEither")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesEnum"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesEnum")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenDict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenDict")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesFrozenList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesFrozenList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesGeneric"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesGeneric")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesJust"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesJust")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesLeft"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesLeft")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesMaybe"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesMaybe")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNode")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesNothing"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesNothing")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesRight"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ext.python.environment.PythonModuleMetadata"),
              Core.projectionField = (Core.Name "usesRight")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "usesTypeVar"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pythonVersionPython310 :: Phantoms.TTerm Environment.PythonVersion
pythonVersionPython310 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.python.environment.PythonVersion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "python310"),
        Core.fieldTerm = Core.TermUnit}}))

pythonVersionPython312 :: Phantoms.TTerm Environment.PythonVersion
pythonVersionPython312 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ext.python.environment.PythonVersion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "python312"),
        Core.fieldTerm = Core.TermUnit}}))
