-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.graph

module Hydra.Dsl.Graph where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.graph.Graph
graph :: Typed.TypedTerm (M.Map Core.Name Core.Term) -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm (M.Map Core.Name Core.TypeVariableConstraints) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm (M.Map Core.Name Core.Term) -> Typed.TypedTerm (M.Map Core.Name Graph.Primitive) -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Graph.Graph
graph boundTerms boundTypes classConstraints lambdaVariables metadata primitives schemaTypes typeVariables =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Typed.unTypedTerm boundTerms)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Typed.unTypedTerm boundTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Typed.unTypedTerm classConstraints)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Typed.unTypedTerm lambdaVariables)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Typed.unTypedTerm primitives)},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Typed.unTypedTerm schemaTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Typed.unTypedTerm typeVariables)}]}))
-- | DSL accessor for the boundTerms field of hydra.graph.Graph
graphBoundTerms :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.Term)
graphBoundTerms x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "boundTerms")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the boundTypes field of hydra.graph.Graph
graphBoundTypes :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme)
graphBoundTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "boundTypes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the classConstraints field of hydra.graph.Graph
graphClassConstraints :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeVariableConstraints)
graphClassConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "classConstraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lambdaVariables field of hydra.graph.Graph
graphLambdaVariables :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (S.Set Core.Name)
graphLambdaVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "lambdaVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the metadata field of hydra.graph.Graph
graphMetadata :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.Term)
graphMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primitives field of hydra.graph.Graph
graphPrimitives :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Graph.Primitive)
graphPrimitives x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "primitives")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the schemaTypes field of hydra.graph.Graph
graphSchemaTypes :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme)
graphSchemaTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "schemaTypes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVariables field of hydra.graph.Graph
graphTypeVariables :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (S.Set Core.Name)
graphTypeVariables x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "typeVariables")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the boundTerms field of hydra.graph.Graph
graphWithBoundTerms :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.Term) -> Typed.TypedTerm Graph.Graph
graphWithBoundTerms original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the boundTypes field of hydra.graph.Graph
graphWithBoundTypes :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm Graph.Graph
graphWithBoundTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the classConstraints field of hydra.graph.Graph
graphWithClassConstraints :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeVariableConstraints) -> Typed.TypedTerm Graph.Graph
graphWithClassConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the lambdaVariables field of hydra.graph.Graph
graphWithLambdaVariables :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Graph.Graph
graphWithLambdaVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the metadata field of hydra.graph.Graph
graphWithMetadata :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.Term) -> Typed.TypedTerm Graph.Graph
graphWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the primitives field of hydra.graph.Graph
graphWithPrimitives :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Graph.Primitive) -> Typed.TypedTerm Graph.Graph
graphWithPrimitives original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the schemaTypes field of hydra.graph.Graph
graphWithSchemaTypes :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (M.Map Core.Name Core.TypeScheme) -> Typed.TypedTerm Graph.Graph
graphWithSchemaTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeVariables field of hydra.graph.Graph
graphWithTypeVariables :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm (S.Set Core.Name) -> Typed.TypedTerm Graph.Graph
graphWithTypeVariables original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.graph.Library
library :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm String -> Typed.TypedTerm [Graph.Primitive] -> Typed.TypedTerm Graph.Library
library name prefix primitives =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Typed.unTypedTerm primitives)}]}))
-- | DSL accessor for the name field of hydra.graph.Library
libraryName :: Typed.TypedTerm Graph.Library -> Typed.TypedTerm Packaging.ModuleName
libraryName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the prefix field of hydra.graph.Library
libraryPrefix :: Typed.TypedTerm Graph.Library -> Typed.TypedTerm String
libraryPrefix x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the primitives field of hydra.graph.Library
libraryPrimitives :: Typed.TypedTerm Graph.Library -> Typed.TypedTerm [Graph.Primitive]
libraryPrimitives x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
        Core.projectionFieldName = (Core.Name "primitives")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.graph.Library
libraryWithName :: Typed.TypedTerm Graph.Library -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Graph.Library
libraryWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the prefix field of hydra.graph.Library
libraryWithPrefix :: Typed.TypedTerm Graph.Library -> Typed.TypedTerm String -> Typed.TypedTerm Graph.Library
libraryWithPrefix original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the primitives field of hydra.graph.Library
libraryWithPrimitives :: Typed.TypedTerm Graph.Library -> Typed.TypedTerm [Graph.Primitive] -> Typed.TypedTerm Graph.Library
libraryWithPrimitives original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.graph.Primitive
primitive :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm (Typing.InferenceContext -> Graph.Graph -> [Core.Term] -> Either Errors.Error Core.Term) -> Typed.TypedTerm Graph.Primitive
primitive definition implementation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Typed.unTypedTerm definition)},
        Core.Field {
          Core.fieldName = (Core.Name "implementation"),
          Core.fieldTerm = (Typed.unTypedTerm implementation)}]}))
-- | DSL accessor for the definition field of hydra.graph.Primitive
primitiveDefinition :: Typed.TypedTerm Graph.Primitive -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinition x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
        Core.projectionFieldName = (Core.Name "definition")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the implementation field of hydra.graph.Primitive
primitiveImplementation :: Typed.TypedTerm Graph.Primitive -> Typed.TypedTerm (Typing.InferenceContext -> Graph.Graph -> [Core.Term] -> Either Errors.Error Core.Term)
primitiveImplementation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
        Core.projectionFieldName = (Core.Name "implementation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the definition field of hydra.graph.Primitive
primitiveWithDefinition :: Typed.TypedTerm Graph.Primitive -> Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Graph.Primitive
primitiveWithDefinition original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
              Core.projectionFieldName = (Core.Name "implementation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the implementation field of hydra.graph.Primitive
primitiveWithImplementation :: Typed.TypedTerm Graph.Primitive -> Typed.TypedTerm (Typing.InferenceContext -> Graph.Graph -> [Core.Term] -> Either Errors.Error Core.Term) -> Typed.TypedTerm Graph.Primitive
primitiveWithImplementation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implementation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.graph.TermCoder
termCoder :: Typed.TypedTerm Core.Type -> Typed.TypedTerm (Typing.InferenceContext -> Graph.Graph -> Core.Term -> Either Errors.Error a) -> Typed.TypedTerm (Typing.InferenceContext -> a -> Either Errors.Error Core.Term) -> Typed.TypedTerm (Graph.TermCoder a)
termCoder type_ encode decode =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.graph.TermCoder
termCoderDecode :: Typed.TypedTerm (Graph.TermCoder a) -> Typed.TypedTerm (Typing.InferenceContext -> a -> Either Errors.Error Core.Term)
termCoderDecode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
        Core.projectionFieldName = (Core.Name "decode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the encode field of hydra.graph.TermCoder
termCoderEncode :: Typed.TypedTerm (Graph.TermCoder a) -> Typed.TypedTerm (Typing.InferenceContext -> Graph.Graph -> Core.Term -> Either Errors.Error a)
termCoderEncode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
        Core.projectionFieldName = (Core.Name "encode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.graph.TermCoder
termCoderType :: Typed.TypedTerm (Graph.TermCoder a) -> Typed.TypedTerm Core.Type
termCoderType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decode field of hydra.graph.TermCoder
termCoderWithDecode :: Typed.TypedTerm (Graph.TermCoder a) -> Typed.TypedTerm (Typing.InferenceContext -> a -> Either Errors.Error Core.Term) -> Typed.TypedTerm (Graph.TermCoder a)
termCoderWithDecode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.graph.TermCoder
termCoderWithEncode :: Typed.TypedTerm (Graph.TermCoder a) -> Typed.TypedTerm (Typing.InferenceContext -> Graph.Graph -> Core.Term -> Either Errors.Error a) -> Typed.TypedTerm (Graph.TermCoder a)
termCoderWithEncode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.graph.TermCoder
termCoderWithType :: Typed.TypedTerm (Graph.TermCoder a) -> Typed.TypedTerm Core.Type -> Typed.TypedTerm (Graph.TermCoder a)
termCoderWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
