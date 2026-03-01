-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.decode.error

module Hydra.Sources.Decode.Error where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

module_ :: Module.Module
module_ = Module.Module {
  Module.moduleNamespace = (Module.Namespace "hydra.decode.error"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.error.decodingError"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "cx"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "raw"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "err"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
                    Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type hydra.error.DecodingError"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "wrap"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "wrappedTerm"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                                  Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
                                  Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))})))})),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "cx"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "raw"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "err"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
                                              Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
                                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "stripped"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                              Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                                              Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
                                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected literal"))}))))),
                                              Core.caseStatementCases = [
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "literal"),
                                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                    Core.lambdaParameter = (Core.Name "v"),
                                                    Core.lambdaDomain = Nothing,
                                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                                        Core.caseStatementTypeName = (Core.Name "hydra.core.Literal"),
                                                        Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                          Core.wrappedTermTypeName = (Core.Name "hydra.error.DecodingError"),
                                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected string literal"))}))))),
                                                        Core.caseStatementCases = [
                                                          Core.Field {
                                                            Core.fieldName = (Core.Name "string"),
                                                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                              Core.lambdaParameter = (Core.Name "s"),
                                                              Core.lambdaDomain = Nothing,
                                                              Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "s"))))})))}]})))),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})))}]})))),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))}))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.core.WrappedTerm"),
                                  Core.projectionField = (Core.Name "body")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "wrappedTerm"))}))}))}))})))}]})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
              Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))}))),
      Core.bindingType = (Just (Core.TypeScheme {
        Core.typeSchemeVariables = [],
        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
            Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.error.DecodingError")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.error.DecodingError"))}))}))})),
        Core.typeSchemeConstraints = Nothing}))}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.extract.helpers",
    (Module.Namespace "hydra.lexical"),
    (Module.Namespace "hydra.rewriting"),
    (Module.Namespace "hydra.decode.context")],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.error",
    (Module.Namespace "hydra.util")],
  Module.moduleDescription = (Just "Term decoders for hydra.error")}
