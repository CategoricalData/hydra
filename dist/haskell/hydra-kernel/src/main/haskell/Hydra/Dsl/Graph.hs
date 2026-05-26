-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.graph

module Hydra.Dsl.Graph where
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.graph.Graph
graph :: Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm (M.Map Core.Name Graph.Primitive) -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Graph.Graph
graph boundTerms boundTypes classConstraints lambdaVariables metadata primitives schemaTypes typeVariables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm typeVariables)}]}))
-- | DSL accessor for the boundTerms field of hydra.graph.Graph
graphBoundTerms :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term)
graphBoundTerms x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "boundTerms")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the boundTypes field of hydra.graph.Graph
graphBoundTypes :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme)
graphBoundTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "boundTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the classConstraints field of hydra.graph.Graph
graphClassConstraints :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata)
graphClassConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "classConstraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the lambdaVariables field of hydra.graph.Graph
graphLambdaVariables :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name)
graphLambdaVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "lambdaVariables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the metadata field of hydra.graph.Graph
graphMetadata :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term)
graphMetadata x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primitives field of hydra.graph.Graph
graphPrimitives :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Graph.Primitive)
graphPrimitives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "primitives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the schemaTypes field of hydra.graph.Graph
graphSchemaTypes :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme)
graphSchemaTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "schemaTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeVariables field of hydra.graph.Graph
graphTypeVariables :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name)
graphTypeVariables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
        Core.projectionFieldName = (Core.Name "typeVariables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the boundTerms field of hydra.graph.Graph
graphWithBoundTerms :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Graph.Graph
graphWithBoundTerms original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the boundTypes field of hydra.graph.Graph
graphWithBoundTypes :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm Graph.Graph
graphWithBoundTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the classConstraints field of hydra.graph.Graph
graphWithClassConstraints :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeVariableMetadata) -> Phantoms.TTerm Graph.Graph
graphWithClassConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the lambdaVariables field of hydra.graph.Graph
graphWithLambdaVariables :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Graph.Graph
graphWithLambdaVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the metadata field of hydra.graph.Graph
graphWithMetadata :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Graph.Graph
graphWithMetadata original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the primitives field of hydra.graph.Graph
graphWithPrimitives :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Graph.Primitive) -> Phantoms.TTerm Graph.Graph
graphWithPrimitives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the schemaTypes field of hydra.graph.Graph
graphWithSchemaTypes :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (M.Map Core.Name Core.TypeScheme) -> Phantoms.TTerm Graph.Graph
graphWithSchemaTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "typeVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeVariables field of hydra.graph.Graph
graphWithTypeVariables :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Graph.Graph
graphWithTypeVariables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "boundTerms"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTerms")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "boundTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "boundTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "classConstraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "classConstraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lambdaVariables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "lambdaVariables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Graph"),
              Core.projectionFieldName = (Core.Name "schemaTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.graph.Library
library :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm String -> Phantoms.TTerm [Graph.Primitive] -> Phantoms.TTerm Graph.Library
library namespace prefix primitives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Phantoms.unTTerm primitives)}]}))
-- | DSL accessor for the namespace field of hydra.graph.Library
libraryNamespace :: Phantoms.TTerm Graph.Library -> Phantoms.TTerm Packaging.ModuleName
libraryNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the prefix field of hydra.graph.Library
libraryPrefix :: Phantoms.TTerm Graph.Library -> Phantoms.TTerm String
libraryPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primitives field of hydra.graph.Library
libraryPrimitives :: Phantoms.TTerm Graph.Library -> Phantoms.TTerm [Graph.Primitive]
libraryPrimitives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
        Core.projectionFieldName = (Core.Name "primitives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the namespace field of hydra.graph.Library
libraryWithNamespace :: Phantoms.TTerm Graph.Library -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Graph.Library
libraryWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the prefix field of hydra.graph.Library
libraryWithPrefix :: Phantoms.TTerm Graph.Library -> Phantoms.TTerm String -> Phantoms.TTerm Graph.Library
libraryWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the primitives field of hydra.graph.Library
libraryWithPrimitives :: Phantoms.TTerm Graph.Library -> Phantoms.TTerm [Graph.Primitive] -> Phantoms.TTerm Graph.Library
libraryWithPrimitives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Library"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.graph.Primitive
primitive :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Context.Context -> Graph.Graph -> [Core.Term] -> Either Errors.Error Core.Term) -> Phantoms.TTerm Graph.Primitive
primitive definition implementation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Phantoms.unTTerm definition)},
        Core.Field {
          Core.fieldName = (Core.Name "implementation"),
          Core.fieldTerm = (Phantoms.unTTerm implementation)}]}))
-- | DSL accessor for the definition field of hydra.graph.Primitive
primitiveDefinition :: Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
        Core.projectionFieldName = (Core.Name "definition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the implementation field of hydra.graph.Primitive
primitiveImplementation :: Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm (Context.Context -> Graph.Graph -> [Core.Term] -> Either Errors.Error Core.Term)
primitiveImplementation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
        Core.projectionFieldName = (Core.Name "implementation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the definition field of hydra.graph.Primitive
primitiveWithDefinition :: Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Graph.Primitive
primitiveWithDefinition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "implementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
              Core.projectionFieldName = (Core.Name "implementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the implementation field of hydra.graph.Primitive
primitiveWithImplementation :: Phantoms.TTerm Graph.Primitive -> Phantoms.TTerm (Context.Context -> Graph.Graph -> [Core.Term] -> Either Errors.Error Core.Term) -> Phantoms.TTerm Graph.Primitive
primitiveWithImplementation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.Primitive"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.Primitive"),
              Core.projectionFieldName = (Core.Name "definition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "implementation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.graph.TermCoder
termCoder :: Phantoms.TTerm Core.Type -> Phantoms.TTerm (Context.Context -> Graph.Graph -> Core.Term -> Either Errors.Error a) -> Phantoms.TTerm (Context.Context -> a -> Either Errors.Error Core.Term) -> Phantoms.TTerm (Graph.TermCoder a)
termCoder type_ encode decode =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
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
          Core.fieldTerm = (Phantoms.unTTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.graph.TermCoder
termCoderDecode :: Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> a -> Either Errors.Error Core.Term)
termCoderDecode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
        Core.projectionFieldName = (Core.Name "decode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the encode field of hydra.graph.TermCoder
termCoderEncode :: Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> Graph.Graph -> Core.Term -> Either Errors.Error a)
termCoderEncode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
        Core.projectionFieldName = (Core.Name "encode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the type field of hydra.graph.TermCoder
termCoderType :: Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm Core.Type
termCoderType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decode field of hydra.graph.TermCoder
termCoderWithDecode :: Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> a -> Either Errors.Error Core.Term) -> Phantoms.TTerm (Graph.TermCoder a)
termCoderWithDecode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.graph.TermCoder
termCoderWithEncode :: Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm (Context.Context -> Graph.Graph -> Core.Term -> Either Errors.Error a) -> Phantoms.TTerm (Graph.TermCoder a)
termCoderWithEncode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the type field of hydra.graph.TermCoder
termCoderWithType :: Phantoms.TTerm (Graph.TermCoder a) -> Phantoms.TTerm Core.Type -> Phantoms.TTerm (Graph.TermCoder a)
termCoderWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graph.TermCoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graph.TermCoder"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
