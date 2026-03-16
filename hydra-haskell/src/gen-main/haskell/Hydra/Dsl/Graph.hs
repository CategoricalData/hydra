-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.graph

module Hydra.Dsl.Graph where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error as Error
import qualified Hydra.Graph as Graph
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

graph :: (Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm (M.Map Core.Name Graph.Primitive) -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Graph.Graph)
graph boundTerms boundTypes classConstraints lambdaVariables metadata primitives schemaTypes typeVariables = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Phantoms.unTTerm boundTerms)},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Phantoms.unTTerm boundTypes)},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Phantoms.unTTerm classConstraints)},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Phantoms.unTTerm lambdaVariables)},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Phantoms.unTTerm metadata)},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Phantoms.unTTerm primitives)},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Phantoms.unTTerm schemaTypes)},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Phantoms.unTTerm typeVariables)}]})))

graphBoundTerms :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term))
graphBoundTerms x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "boundTerms")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphBoundTypes :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme))
graphBoundTypes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "boundTypes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphClassConstraints :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata))
graphClassConstraints x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "classConstraints")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphLambdaVariables :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name))
graphLambdaVariables x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "lambdaVariables")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphMetadata :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term))
graphMetadata x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "metadata")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphPrimitives :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Graph.Primitive))
graphPrimitives x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "primitives")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphSchemaTypes :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme))
graphSchemaTypes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "schemaTypes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphTypeVariables :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name))
graphTypeVariables x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
    Core.projectionField = (Core.Name "typeVariables")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphWithBoundTerms :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Graph.Graph)
graphWithBoundTerms original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithBoundTypes :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm Graph.Graph)
graphWithBoundTypes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithClassConstraints :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm Graph.Graph)
graphWithClassConstraints original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithLambdaVariables :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Graph.Graph)
graphWithLambdaVariables original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithMetadata :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Graph.Graph)
graphWithMetadata original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithPrimitives :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Graph.Primitive) -> Phantoms.TTerm Graph.Graph)
graphWithPrimitives original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithSchemaTypes :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm Graph.Graph)
graphWithSchemaTypes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "typeVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithTypeVariables :: (Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Graph.Graph)
graphWithTypeVariables original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "boundTerms"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTerms")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "boundTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "boundTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "classConstraints"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "classConstraints")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "lambdaVariables"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "lambdaVariables")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "metadata"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "metadata")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "primitives"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "primitives")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "schemaTypes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
          Core.projectionField = (Core.Name "schemaTypes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "typeVariables"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

primitive :: (Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm (Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term) -> Phantoms.TTerm Graph.Primitive)
primitive name type_ implementation = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm name)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "implementation"),
      Core.fieldTerm = (Phantoms.unTTerm implementation)}]})))

primitiveName :: (Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm Core.Name)
primitiveName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
    Core.projectionField = (Core.Name "name")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

primitiveType :: (Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm Core.TypeScheme)
primitiveType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

primitiveImplementation :: (Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm (Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term))
primitiveImplementation x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
    Core.projectionField = (Core.Name "implementation")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

primitiveWithName :: (Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Graph.Primitive)
primitiveWithName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "implementation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
          Core.projectionField = (Core.Name "implementation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

primitiveWithType :: (Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Graph.Primitive)
primitiveWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "implementation"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
          Core.projectionField = (Core.Name "implementation")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

primitiveWithImplementation :: (Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm (Context.Context -> Graph.Graph -> [Core.Term] -> Either (Context.InContext Error.Error) Core.Term) -> Phantoms.TTerm Graph.Primitive)
primitiveWithImplementation original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
          Core.projectionField = (Core.Name "name")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "implementation"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

termCoder :: (Phantoms.TTerm Core.Type -> Phantoms.TTerm (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) a) -> Phantoms.TTerm (Context.Context -> a -> Either (Context.InContext Error.Error) Core.Term) -> Phantoms.TTerm (Graph.TermCoder a))
termCoder type_ encode decode = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "encode"),
      Core.fieldTerm = (Phantoms.unTTerm encode)},
    Core.Field {
      Core.fieldName = (Core.Name "decode"),
      Core.fieldTerm = (Phantoms.unTTerm decode)}]})))

termCoderType :: (Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm Core.Type)
termCoderType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termCoderEncode :: (Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) a))
termCoderEncode x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
    Core.projectionField = (Core.Name "encode")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termCoderDecode :: (Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> a -> Either (Context.InContext Error.Error) Core.Term))
termCoderDecode x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
    Core.projectionField = (Core.Name "decode")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

termCoderWithType :: (Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm Core.Type -> Phantoms.TTerm (Graph.TermCoder a))
termCoderWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "encode"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
          Core.projectionField = (Core.Name "encode")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decode"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
          Core.projectionField = (Core.Name "decode")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

termCoderWithEncode :: (Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> Graph.Graph -> Core.Term -> Either (Context.InContext Error.Error) a) -> Phantoms.TTerm (Graph.TermCoder a))
termCoderWithEncode original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "encode"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "decode"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
          Core.projectionField = (Core.Name "decode")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

termCoderWithDecode :: (Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> a -> Either (Context.InContext Error.Error) Core.Term) -> Phantoms.TTerm (Graph.TermCoder a))
termCoderWithDecode original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "encode"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
          Core.projectionField = (Core.Name "encode")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "decode"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))
