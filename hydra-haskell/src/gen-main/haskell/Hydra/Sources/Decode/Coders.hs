-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.decode.coders

module Hydra.Sources.Decode.Coders where

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
  Module.moduleNamespace = (Module.Namespace "hydra.decode.coders"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.coders.coderDirection"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union of type hydra.coders.CoderDirection"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "union"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "inj"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "tname"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "typeName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "field"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "field")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fname"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fterm"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "term")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "variantMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                                  Core.applicationArgument = (Core.TermList [
                                    Core.TermPair (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "encode"))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "input"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "t"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "encode"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeUnit")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                    (Core.TermPair (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "decode"))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "input"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "t"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "decode"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeUnit")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))))])})),
                                Core.bindingType = Nothing}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                                  Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                      Core.applicationArgument = (Core.TermList [
                                        Core.TermLiteral (Core.LiteralString "no such field "),
                                        Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))}),
                                        Core.TermLiteral (Core.LiteralString " in union type "),
                                        (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "tname"))}))])}))}))))})),
                                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "f"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))})))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))})))}]})))),
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
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.coders.CoderDirection"))}))}))})),
        Core.typeSchemeConstraints = Nothing}))},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.coders.languageName"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type hydra.coders.LanguageName"))}))))),
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
                                  Core.wrappedTermTypeName = (Core.Name "hydra.coders.LanguageName"),
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
              Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.coders.LanguageName"))}))}))})),
        Core.typeSchemeConstraints = Nothing}))},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.coders.traversalOrder"),
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union of type hydra.coders.TraversalOrder"))}))))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "union"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "inj"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLet (Core.Let {
                            Core.letBindings = [
                              Core.Binding {
                                Core.bindingName = (Core.Name "tname"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "typeName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "field"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Injection"),
                                    Core.projectionField = (Core.Name "field")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "inj"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fname"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "fterm"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                    Core.projectionField = (Core.Name "term")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "field"))})),
                                Core.bindingType = Nothing},
                              Core.Binding {
                                Core.bindingName = (Core.Name "variantMap"),
                                Core.bindingTerm = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                                  Core.applicationArgument = (Core.TermList [
                                    Core.TermPair (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "pre"))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "input"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "t"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "pre"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeUnit")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                    (Core.TermPair (Core.TermWrap (Core.WrappedTerm {
                                      Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "post"))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "input"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "t"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                              Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
                                              Core.injectionField = Core.Field {
                                                Core.fieldName = (Core.Name "post"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.helpers.decodeUnit")),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))))])})),
                                Core.bindingType = Nothing}],
                            Core.letBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                                  Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                                    Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                      Core.applicationArgument = (Core.TermList [
                                        Core.TermLiteral (Core.LiteralString "no such field "),
                                        Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))}),
                                        Core.TermLiteral (Core.LiteralString " in union type "),
                                        (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "tname"))}))])}))}))))})),
                                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "f"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "fterm"))}))})))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "variantMap"))}))}))}))})))}]})))),
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
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.coders.TraversalOrder"))}))}))})),
        Core.typeSchemeConstraints = Nothing}))}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.extract.helpers",
    Module.Namespace "hydra.lexical",
    Module.Namespace "hydra.rewriting",
    Module.Namespace "hydra.decode.core",
    Module.Namespace "hydra.decode.graph",
    Module.Namespace "hydra.decode.compute",
    (Module.Namespace "hydra.decode.variants")],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.coders",
    (Module.Namespace "hydra.util")],
  Module.moduleDescription = (Just "Term decoders for hydra.coders")}
