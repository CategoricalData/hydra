-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.parsing

module Hydra.Sources.Encode.Parsing where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.encode.parsing"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term encoders for hydra.parsing"),
        Packaging.entityMetadataComments = [],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.moduleDependencies = [
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.encode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.parsing"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.parsing.parseError"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.parsing.ParseError")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseError"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "message"))}))},
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
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
                                      Core.projectionFieldName = (Core.Name "message")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "remainder"))}))},
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
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.parsing.ParseError"),
                                      Core.projectionFieldName = (Core.Name "remainder")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.parsing.ParseError")))]))}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.parsing.parseResult"),
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
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.parsing.ParseResult")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))})),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "hydra.parsing.ParseResult"),
                Core.caseStatementDefault = Nothing,
                Core.caseStatementCases = [
                  Core.CaseAlternative {
                    Core.caseAlternativeName = (Core.Name "success"),
                    Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseResult"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "field"),
                                Core.fieldTerm = (Core.TermRecord (Core.Record {
                                  Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                  Core.recordFields = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "name"),
                                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "success"))}))},
                                    Core.Field {
                                      Core.fieldName = (Core.Name "term"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parseSuccess")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                  Core.CaseAlternative {
                    Core.caseAlternativeName = (Core.Name "failure"),
                    Core.caseAlternativeHandler = (Core.TermLambda (Core.Lambda {
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseResult"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "field"),
                                Core.fieldTerm = (Core.TermRecord (Core.Record {
                                  Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                  Core.recordFields = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "name"),
                                      Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "failure"))}))},
                                    Core.Field {
                                      Core.fieldName = (Core.Name "term"),
                                      Core.fieldTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.parsing.parseError")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.parsing.ParseResult")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.parsing.parseSuccess"),
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
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.parsing.ParseSuccess")),
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
                            Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.parsing.ParseSuccess"))}))},
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
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "value"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermProject (Core.Projection {
                                        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
                                        Core.projectionFieldName = (Core.Name "value")})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                            (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "remainder"))}))},
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
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermProject (Core.Projection {
                                        Core.projectionTypeName = (Core.Name "hydra.parsing.ParseSuccess"),
                                        Core.projectionFieldName = (Core.Name "remainder")})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Encoder for hydra.parsing.ParseSuccess")))]))}))}))]}
