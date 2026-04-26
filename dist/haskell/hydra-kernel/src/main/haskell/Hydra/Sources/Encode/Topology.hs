-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.topology

module Hydra.Sources.Encode.Topology where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.encode.topology"),
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.topology.graph"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "m"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "map"),
                Core.fieldTerm = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.bimap")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "xs"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermInject (Core.Injection {
                        Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "list"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.topology.Vertex")),
                Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.topology.Vertex")))})),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.topology.tarjanState"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "record"),
                Core.fieldTerm = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "hydra.core.Record"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "typeName"),
                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.topology.TarjanState"))}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "fields"),
                      Core.fieldTerm = (Core.TermList [
                        Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "counter"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "literal"),
                                      Core.fieldTerm = (Core.TermInject (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "integer"),
                                          Core.fieldTerm = (Core.TermInject (Core.Injection {
                                            Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                            Core.injectionField = Core.Field {
                                              Core.fieldName = (Core.Name "int32"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
                                    Core.projectionField = (Core.Name "counter")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "indices"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "m"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "map"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.bimap")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "x"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermInject (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "literal"),
                                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "integer"),
                                                    Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "int32"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
                                    Core.projectionField = (Core.Name "indices")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lowLinks"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "m"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "map"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.bimap")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "x"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermInject (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "literal"),
                                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "integer"),
                                                    Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                                      Core.injectionField = Core.Field {
                                                        Core.fieldName = (Core.Name "int32"),
                                                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
                                    Core.projectionField = (Core.Name "lowLinks")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "stack"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
                                    Core.projectionField = (Core.Name "stack")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "onStack"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "s"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "set"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
                                    Core.projectionField = (Core.Name "onStack")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "sccs"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "xs"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermInject (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "list"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                          Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "xs"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermInject (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "list"),
                                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.topology.vertex"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermProject (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.topology.TarjanState"),
                                    Core.projectionField = (Core.Name "sccs")})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.topology.TarjanState")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.topology.vertex"),
          Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermInject (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.core.Term"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "literal"),
                Core.fieldTerm = (Core.TermInject (Core.Injection {
                  Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "integer"),
                    Core.fieldTerm = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "int32"),
                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Literal")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}))],
      Packaging.moduleTermDependencies = [
        Packaging.Namespace "hydra.encode.core"],
      Packaging.moduleTypeDependencies = [
        Packaging.Namespace "hydra.topology"],
      Packaging.moduleDescription = (Just "Term encoders for hydra.topology")}
