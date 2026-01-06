-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.decode.compute

module Hydra.Sources.Decode.Compute where

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
  Module.moduleNamespace = (Module.Namespace "hydra.decode.compute"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.compute.flowState"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "s"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "v"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                        Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "stripped"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                        Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record of type hydra.compute.FlowState"))}))))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "record"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "record"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLet (Core.Let {
                                Core.letBindings = [
                                  Core.Binding {
                                    Core.bindingName = (Core.Name "fieldMap"),
                                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.toFieldMap")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "record"))})),
                                    Core.bindingType = Nothing}],
                                Core.letBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "value"))})),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeMaybe")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "v"))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "field_value"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "state"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "field_state"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                                                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "trace"))})),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.compute.trace"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "field_trace"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
                                              Core.recordFields = [
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "value"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_value"))},
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "state"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_state"))},
                                                Core.Field {
                                                  Core.fieldName = (Core.Name "trace"),
                                                  Core.fieldTerm = (Core.TermVariable (Core.Name "field_trace"))}]}))))})))}))})))}))})))}))}))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))})))}))),
      Core.bindingType = (Just (Core.TypeScheme {
        Core.typeSchemeVariables = [
          Core.Name "s",
          (Core.Name "v")],
        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "s"))}))}))})),
          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "v"))}))}))})),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                  Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.FlowState")),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "v"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "s"))}))}))}))}))}))})),
        Core.typeSchemeConstraints = Nothing}))},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.compute.trace"),
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
                    Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                    Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "stripped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                    Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record of type hydra.compute.Trace"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "record"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "record"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "fieldMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.toFieldMap")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "record"))})),
                                Core.bindingType = Nothing}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "stack"))})),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeList")),
                                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                                                      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                                                      Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
                                                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                  Core.lambdaParameter = (Core.Name "stripped"),
                                                  Core.lambdaDomain = Nothing,
                                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                                      Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                                                      Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
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
                                                                  Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
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
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "field_stack"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "messages"))})),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeList")),
                                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                                                          Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                                                          Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
                                                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                      Core.lambdaParameter = (Core.Name "stripped"),
                                                      Core.lambdaDomain = Nothing,
                                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                                          Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                                                          Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                                            Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
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
                                                                      Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
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
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))}))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "field_messages"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.requireField")),
                                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "other"))})),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeMap")),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.name"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.decode.core.term"))}))})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))}))})),
                                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "field_other"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                          Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                                          Core.recordFields = [
                                            Core.Field {
                                              Core.fieldName = (Core.Name "stack"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "field_stack"))},
                                            Core.Field {
                                              Core.fieldName = (Core.Name "messages"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "field_messages"))},
                                            Core.Field {
                                              Core.fieldName = (Core.Name "other"),
                                              Core.fieldTerm = (Core.TermVariable (Core.Name "field_other"))}]}))))})))}))})))}))})))}))}))})))}]})))),
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
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.compute.Trace"))}))}))})),
        Core.typeSchemeConstraints = Nothing}))}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.extract.helpers",
    Module.Namespace "hydra.lexical",
    Module.Namespace "hydra.rewriting",
    (Module.Namespace "hydra.decode.core")],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.compute",
    (Module.Namespace "hydra.util")],
  Module.moduleDescription = (Just "Term decoders for hydra.compute")}
