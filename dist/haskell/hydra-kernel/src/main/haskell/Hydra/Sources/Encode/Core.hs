-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.core

module Hydra.Sources.Encode.Core where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.encode.core"),
      Packaging.moduleDescription = (Just "Term encoders for hydra.core"),
      Packaging.moduleDependencies = [
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.encode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.core"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.annotatedTerm"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.AnnotatedTerm"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotation"))}))},
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
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedTerm"),
                                      Core.projectionFieldName = (Core.Name "annotation")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.AnnotatedTerm")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.AnnotatedTerm")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.annotatedType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.AnnotatedType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotation"))}))},
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
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.AnnotatedType"),
                                      Core.projectionFieldName = (Core.Name "annotation")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.AnnotatedType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.AnnotatedType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.application"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Application"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "function"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Application"),
                                      Core.projectionFieldName = (Core.Name "function")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "argument"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Application"),
                                      Core.projectionFieldName = (Core.Name "argument")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Application")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Application")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.applicationType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.ApplicationType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "function"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
                                      Core.projectionFieldName = (Core.Name "function")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "argument"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.ApplicationType"),
                                      Core.projectionFieldName = (Core.Name "argument")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.ApplicationType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.ApplicationType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.binding"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Binding"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
                                      Core.projectionFieldName = (Core.Name "name")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "term"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
                                      Core.projectionFieldName = (Core.Name "term")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeScheme"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "opt"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "maybe"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.typeScheme"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Binding"),
                                      Core.projectionFieldName = (Core.Name "typeScheme")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Binding")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Binding")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.caseStatement"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.CaseStatement"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
                                      Core.projectionFieldName = (Core.Name "typeName")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "default"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "opt"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "maybe"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
                                      Core.projectionFieldName = (Core.Name "default")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "cases"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.field"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.CaseStatement"),
                                      Core.projectionFieldName = (Core.Name "cases")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.CaseStatement")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.CaseStatement")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.eitherType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.EitherType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "left"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
                                      Core.projectionFieldName = (Core.Name "left")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "right"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.EitherType"),
                                      Core.projectionFieldName = (Core.Name "right")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.EitherType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.EitherType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.field"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Field"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                      Core.projectionFieldName = (Core.Name "name")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "term"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                      Core.projectionFieldName = (Core.Name "term")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Field")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Field")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.fieldType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FieldType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
                                      Core.projectionFieldName = (Core.Name "name")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "type"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.FieldType"),
                                      Core.projectionFieldName = (Core.Name "type")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.FieldType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.FieldType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.floatType"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.FloatType"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "float32"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float32"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "float64"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float64"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.FloatType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.FloatType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.floatValue"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.FloatValue"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "float32"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float32"))}))},
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
                                                Core.fieldName = (Core.Name "float"),
                                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "float32"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "float64"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FloatValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float64"))}))},
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
                                                Core.fieldName = (Core.Name "float"),
                                                Core.fieldTerm = (Core.TermInject (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.core.FloatValue"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "float64"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.FloatValue")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.FloatValue")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.forallType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.ForallType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "parameter"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
                                      Core.projectionFieldName = (Core.Name "parameter")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.ForallType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.ForallType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.functionType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.FunctionType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "domain"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
                                      Core.projectionFieldName = (Core.Name "domain")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "codomain"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.FunctionType"),
                                      Core.projectionFieldName = (Core.Name "codomain")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.FunctionType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.FunctionType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.injection"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Injection"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                      Core.projectionFieldName = (Core.Name "typeName")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "field"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.field")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                      Core.projectionFieldName = (Core.Name "field")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Injection")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Injection")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.integerType"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerType"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "bigint"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bigint"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int8"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int8"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int16"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int16"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int32"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int32"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int64"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int64"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint8"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint8"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint16"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint16"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint32"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint32"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint64"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint64"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.IntegerType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.IntegerType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.integerValue"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.IntegerValue"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "bigint"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bigint"))}))},
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
                                                    Core.fieldName = (Core.Name "bigint"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int8"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int8"))}))},
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
                                                    Core.fieldName = (Core.Name "int8"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int16"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int16"))}))},
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
                                                    Core.fieldName = (Core.Name "int16"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int32"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int32"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "int64"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "int64"))}))},
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
                                                    Core.fieldName = (Core.Name "int64"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint8"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint8"))}))},
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
                                                    Core.fieldName = (Core.Name "uint8"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint16"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint16"))}))},
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
                                                    Core.fieldName = (Core.Name "uint16"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint32"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint32"))}))},
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
                                                    Core.fieldName = (Core.Name "uint32"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "uint64"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.IntegerValue"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "uint64"))}))},
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
                                                    Core.fieldName = (Core.Name "uint64"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.IntegerValue")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.IntegerValue")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.lambda"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Lambda"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "parameter"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
                                      Core.projectionFieldName = (Core.Name "parameter")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "domain"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "opt"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "maybe"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.type"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
                                      Core.projectionFieldName = (Core.Name "domain")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Lambda"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Lambda")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Lambda")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.let"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Let"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bindings"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.binding"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Let"),
                                      Core.projectionFieldName = (Core.Name "bindings")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Let"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Let")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Let")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.literal"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "binary"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "binary"))}))},
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
                                                Core.fieldName = (Core.Name "binary"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "boolean"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "boolean"))}))},
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
                                                Core.fieldName = (Core.Name "boolean"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "decimal"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decimal"))}))},
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
                                                Core.fieldName = (Core.Name "decimal"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.floatValue")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "integer"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "integer"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.integerValue")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "string"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Literal"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "string"))}))},
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
                                                Core.fieldName = (Core.Name "string"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Literal")))])})),
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
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.literalType"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.LiteralType"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "binary"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "binary"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "boolean"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "boolean"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "decimal"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decimal"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "float"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.floatType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "integer"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "integer"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.integerType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "string"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.LiteralType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "string"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.LiteralType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.LiteralType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.mapType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.MapType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "keys"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
                                      Core.projectionFieldName = (Core.Name "keys")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "values"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.MapType"),
                                      Core.projectionFieldName = (Core.Name "values")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.MapType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.MapType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.name"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermInject (Core.Injection {
                Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "typeName"),
                        Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Name"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "body"),
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
                                    Core.fieldName = (Core.Name "string"),
                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.core.Name")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Name")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.pairType"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.PairType"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "first"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
                                      Core.projectionFieldName = (Core.Name "first")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "second"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.PairType"),
                                      Core.projectionFieldName = (Core.Name "second")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.PairType")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.PairType")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.projection"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Projection"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
                                      Core.projectionFieldName = (Core.Name "typeName")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "fieldName"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Projection"),
                                      Core.projectionFieldName = (Core.Name "fieldName")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Projection")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Projection")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.record"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Record"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Record"),
                                      Core.projectionFieldName = (Core.Name "typeName")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "fields"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.field"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.Record"),
                                      Core.projectionFieldName = (Core.Name "fields")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Record")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Record")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.term"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "annotated"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotated"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.annotatedTerm")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "application"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "application"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.application")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "cases"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "cases"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.caseStatement")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "either"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "either"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "e"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "either"),
                                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.eithers.bimap")),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "inject"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "inject"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.injection")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "lambda"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lambda"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.lambda")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "let"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "let"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.let")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "list"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "list"))}))},
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "literal"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "literal"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.literal")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "map"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "map"))}))},
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
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "maybe"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "maybe"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "opt"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "maybe"),
                                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "pair"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pair"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "p"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "pair"),
                                            Core.fieldTerm = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.bimap")),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "project"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "project"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.projection")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "record"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "record"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.record")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "set"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "set"))}))},
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.term"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "typeApplication"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeApplication"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.typeApplicationTerm")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "typeLambda"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeLambda"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.typeLambda")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "unit"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unit"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "unwrap"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unwrap"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "variable"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "variable"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrap"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.wrappedTerm")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Term")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.type"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.Type"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "annotated"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "annotated"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.annotatedType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "application"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "application"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.applicationType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "either"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "either"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.eitherType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "forall"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "forall"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.forallType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "function"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "function"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.functionType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "list"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "list"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "literal"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "literal"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.literalType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "map"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "map"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.mapType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "maybe"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "maybe"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "pair"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pair"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.pairType")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "record"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "record"))}))},
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.fieldType"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "set"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "set"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "union"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "union"))}))},
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.fieldType"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "unit"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unit"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "variable"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "variable"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "void"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "void"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrap"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.Type")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeApplicationTerm"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeApplicationTerm"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "type"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeApplicationTerm"),
                                      Core.projectionFieldName = (Core.Name "type")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.TypeApplicationTerm")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.TypeApplicationTerm")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeClassConstraint"),
          Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.core.TypeClassConstraint"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "simple"),
                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "inject"),
                        Core.fieldTerm = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "typeName"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeClassConstraint"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "simple"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.TypeClassConstraint")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.TypeClassConstraint")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeLambda"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeLambda"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "parameter"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
                                      Core.projectionFieldName = (Core.Name "parameter")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeLambda"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.TypeLambda")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.TypeLambda")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeScheme"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeScheme"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "variables"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
                                      Core.projectionFieldName = (Core.Name "variables")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "constraints"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "opt"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "maybe"),
                                        Core.fieldTerm = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.map")),
                                            Core.applicationArgument = (Core.TermLambda (Core.Lambda {
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
                                                        Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.name"))})),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.typeVariableMetadata"))})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))}}))}))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeScheme"),
                                      Core.projectionFieldName = (Core.Name "constraints")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.TypeScheme")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.TypeScheme")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.typeVariableMetadata"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.TypeVariableMetadata"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "classes"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.core.typeClassConstraint"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.TypeVariableMetadata"),
                                      Core.projectionFieldName = (Core.Name "classes")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.TypeVariableMetadata")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.TypeVariableMetadata")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.core.wrappedTerm"),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.WrappedTerm"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeName"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                      Core.projectionFieldName = (Core.Name "typeName")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "body"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                      Core.projectionFieldName = (Core.Name "body")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.core.WrappedTerm")))])})),
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.core.WrappedTerm")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}}))}))]}
