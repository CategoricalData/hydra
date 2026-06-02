-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.topology

module Hydra.Sources.Encode.Topology where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.encode.topology"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term encoders for hydra.topology"),
        Packaging.entityMetadataComments = [],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.moduleDependencies = [
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.encode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.topology"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.topology.graph"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
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
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.topology.Graph")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.topology.Vertex")),
                  Core.mapTypeValues = (Core.TypeList (Core.TypeVariable (Core.Name "hydra.topology.Vertex")))})),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.topology.tarjanState"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
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
                                      Core.projectionFieldName = (Core.Name "counter")})),
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
                                      Core.projectionFieldName = (Core.Name "indices")})),
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
                                      Core.projectionFieldName = (Core.Name "lowLinks")})),
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
                                      Core.projectionFieldName = (Core.Name "stack")})),
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
                                      Core.projectionFieldName = (Core.Name "onStack")})),
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
                                      Core.projectionFieldName = (Core.Name "sccs")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.topology.TarjanState")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.topology.TarjanState")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.topology.vertex"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
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
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.topology.Vertex")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Literal")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))}))]}
