-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.typed

module Hydra.Sources.Encode.Typed where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.encode.typed"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term encoders for hydra.typed"),
        Packaging.entityMetadataComments = [],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.moduleDependencies = [
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.encode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.typed"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.typed.typedBinding"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [
              Typing.TypeParameter {
                Typing.typeParameterName = (Core.Name "a"),
                Typing.typeParameterConstraints = []}],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedBinding")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.typed.TypedBinding"))}))},
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
                                        Core.projectionTypeName = (Core.Name "hydra.typed.TypedBinding"),
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
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.typed.typedTerm")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermProject (Core.Projection {
                                        Core.projectionTypeName = (Core.Name "hydra.typed.TypedBinding"),
                                        Core.projectionFieldName = (Core.Name "term")})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.typed.TypedBinding")))])}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.typed.typedTerm"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [
              Typing.TypeParameter {
                Typing.typeParameterName = (Core.Name "a"),
                Typing.typeParameterConstraints = []}],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTerm")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.typed.TypedTerm"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "body"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typed.TypedTerm")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.typed.TypedTerm")))])}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.typed.typedTermDefinition"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [
              Typing.TypeParameter {
                Typing.typeParameterName = (Core.Name "a"),
                Typing.typeParameterConstraints = []}],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
                Typing.parameterIsLazy = False},
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg1"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.typed.TypedTermDefinition")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.typed.TypedTermDefinition"))}))},
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
                                        Core.projectionTypeName = (Core.Name "hydra.typed.TypedTermDefinition"),
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
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.typed.typedTerm")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermProject (Core.Projection {
                                        Core.projectionTypeName = (Core.Name "hydra.typed.TypedTermDefinition"),
                                        Core.projectionFieldName = (Core.Name "term")})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))})),
            Core.annotatedTermAnnotation = (M.fromList [
              (Core.Name "description", (Core.TermLiteral (Core.LiteralString "Encoder for hydra.typed.TypedTermDefinition")))])}))}))]}
