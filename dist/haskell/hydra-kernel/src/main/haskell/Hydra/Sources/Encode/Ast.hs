-- Note: this is an automatically generated file. Do not edit.
-- | Source module for hydra.encode.ast

module Hydra.Sources.Encode.Ast where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleName = (Packaging.ModuleName "hydra.encode.ast"),
      Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
        Packaging.entityMetadataDescription = (Just "Term encoders for hydra.ast"),
        Packaging.entityMetadataComments = [],
        Packaging.entityMetadataSeeAlso = [],
        Packaging.entityMetadataLifecycle = Nothing})),
      Packaging.moduleDependencies = [
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.encode.core"),
          Packaging.moduleDependencyPackage = Nothing},
        Packaging.ModuleDependency {
          Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.ast"),
          Packaging.moduleDependencyPackage = Nothing}],
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.associativity"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Associativity")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.ast.Associativity"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "none"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Associativity"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "none"))}))},
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
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "left"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Associativity"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
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
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "right"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Associativity"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
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
                                      Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "_"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermInject (Core.Injection {
                                          Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                          Core.injectionField = Core.Field {
                                            Core.fieldName = (Core.Name "unit"),
                                            Core.fieldTerm = Core.TermUnit}}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "both"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Associativity"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "both"))}))},
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Associativity")))]))}))}),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.blockStyle"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.BlockStyle")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.BlockStyle"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "indent"))}))},
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
                                                      Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "opt"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
                                      Core.projectionFieldName = (Core.Name "indent")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "newlineBeforeContent"))}))},
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
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
                                      Core.projectionFieldName = (Core.Name "newlineBeforeContent")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "newlineAfterContent"))}))},
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
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
                                      Core.projectionFieldName = (Core.Name "newlineAfterContent")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.BlockStyle")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.bracketExpr"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.BracketExpr")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.BracketExpr"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "brackets"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.brackets")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
                                      Core.projectionFieldName = (Core.Name "brackets")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "enclosed"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.expr")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
                                      Core.projectionFieldName = (Core.Name "enclosed")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "style"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.blockStyle")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
                                      Core.projectionFieldName = (Core.Name "style")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.BracketExpr")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.brackets"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Brackets")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Brackets"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "open"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.symbol")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
                                      Core.projectionFieldName = (Core.Name "open")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "close"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.symbol")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
                                      Core.projectionFieldName = (Core.Name "close")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Brackets")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.expr"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Expr")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.ast.Expr"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "const"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Expr"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "const"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.symbol")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "indent"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Expr"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "indent"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.indentedExpression")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "op"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Expr"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "op"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.opExpr")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "brackets"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Expr"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "brackets"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.bracketExpr")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "seq"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Expr"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "seq"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "term"),
                                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.seqExpr")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))}]})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Expr")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.indentStyle"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.IndentStyle")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.ast.IndentStyle"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "allLines"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.IndentStyle"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "allLines"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "subsequentLines"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.IndentStyle"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "subsequentLines"))}))},
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.IndentStyle")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.indentedExpression"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.IndentedExpression")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.IndentedExpression"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "style"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.indentStyle")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
                                      Core.projectionFieldName = (Core.Name "style")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expr"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.expr")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
                                      Core.projectionFieldName = (Core.Name "expr")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (
                Core.TermVariable (Core.Name "description"),
                (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.IndentedExpression")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.op"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Op")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Op"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "symbol"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.symbol")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
                                      Core.projectionFieldName = (Core.Name "symbol")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "padding"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.padding")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
                                      Core.projectionFieldName = (Core.Name "padding")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "precedence"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.precedence")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
                                      Core.projectionFieldName = (Core.Name "precedence")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "associativity"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.associativity")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
                                      Core.projectionFieldName = (Core.Name "associativity")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Op")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.opExpr"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.OpExpr")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.OpExpr"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "op"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.op")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
                                      Core.projectionFieldName = (Core.Name "op")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "lhs"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.expr")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
                                      Core.projectionFieldName = (Core.Name "lhs")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "rhs"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.expr")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
                                      Core.projectionFieldName = (Core.Name "rhs")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.OpExpr")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.padding"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Padding")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Padding"))}))},
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
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.ws")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
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
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.ws")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
                                      Core.projectionFieldName = (Core.Name "right")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Padding")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.precedence"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Precedence")),
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
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "typeName"),
                        Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Precedence"))}))},
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
                                    Core.fieldName = (Core.Name "integer"),
                                    Core.fieldTerm = (Core.TermInject (Core.Injection {
                                      Core.injectionTypeName = (Core.Name "hydra.core.IntegerValue"),
                                      Core.injectionField = Core.Field {
                                        Core.fieldName = (Core.Name "int32"),
                                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}}))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.ast.Precedence")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Precedence")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.seqExpr"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.SeqExpr")),
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
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.SeqExpr"))}))},
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
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "op"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "term"),
                                Core.fieldTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.ast.op")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
                                      Core.projectionFieldName = (Core.Name "op")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                          (Core.TermRecord (Core.Record {
                            Core.recordTypeName = (Core.Name "hydra.core.Field"),
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "name"),
                                Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "elements"))}))},
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
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.encode.ast.expr"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))}}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                      Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
                                      Core.projectionFieldName = (Core.Name "elements")})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.SeqExpr")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.symbol"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Symbol")),
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
                  Core.fieldName = (Core.Name "wrap"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.core.WrappedTerm"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "typeName"),
                        Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Symbol"))}))},
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
                            Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.ast.Symbol")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})),
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Symbol")))]))}))})),
        (Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.encode.ast.ws"),
          Packaging.termDefinitionMetadata = Nothing,
          Packaging.termDefinitionSignature = (Just (Typing.TermSignature {
            Typing.termSignatureTypeParameters = [],
            Typing.termSignatureParameters = [
              Typing.Parameter {
                Typing.parameterName = (Core.Name "arg0"),
                Typing.parameterDescription = Nothing,
                Typing.parameterType = (Core.TypeVariable (Core.Name "hydra.ast.Ws")),
                Typing.parameterIsLazy = False}],
            Typing.termSignatureResult = Typing.Result {
              Typing.resultDescription = Nothing,
              Typing.resultType = (Core.TypeVariable (Core.Name "hydra.core.Term"))}})),
          Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "hydra.ast.Ws"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "none"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Ws"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "none"))}))},
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
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "space"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Ws"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "space"))}))},
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
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "break"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Ws"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "break"))}))},
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
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "breakAndIndent"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Ws"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "breakAndIndent"))}))},
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
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))}))},
                Core.CaseAlternative {
                  Core.caseAlternativeName = (Core.Name "doubleBreak"),
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.ast.Ws"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "field"),
                              Core.fieldTerm = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = (Core.Name "hydra.core.Field"),
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "name"),
                                    Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "doubleBreak"))}))},
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
            Core.annotatedTermAnnotation = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "description"), (Core.TermLiteral (Core.LiteralString "Encoder for hydra.ast.Ws")))]))}))}))]}
