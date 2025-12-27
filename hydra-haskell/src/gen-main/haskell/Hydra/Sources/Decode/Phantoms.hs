-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.decode.phantoms

module Hydra.Sources.Decode.Phantoms where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

module_ :: Module.Module
module_ = Module.Module {
  Module.moduleNamespace = (Module.Namespace "hydra.decode.phantoms"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.phantoms.tBinding"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "a"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected record of type hydra.phantoms.TBinding"))}))))),
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
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "f"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermPair (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                              Core.projectionField = (Core.Name "name")})))),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}), (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.core.Field"),
                                              Core.projectionField = (Core.Name "term")})))),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))))})))})),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                          Core.projectionTypeName = (Core.Name "hydra.core.Record"),
                                          Core.projectionField = (Core.Name "fields")})))),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "record"))}))}))})),
                                  Core.bindingType = Nothing}],
                              Core.letBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "err"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))})))})),
                                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "field_name"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "err"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "err"))))})))})),
                                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "field_term"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
                                            Core.recordTypeName = (Core.Name "hydra.phantoms.TBinding"),
                                            Core.recordFields = [
                                              Core.Field {
                                                Core.fieldName = (Core.Name "name"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_name"))},
                                              Core.Field {
                                                Core.fieldName = (Core.Name "term"),
                                                Core.fieldTerm = (Core.TermVariable (Core.Name "field_term"))}]}))))})))})),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                                            Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                                              Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                                Core.applicationArgument = (Core.TermList [
                                                  Core.TermLiteral (Core.LiteralString "missing field "),
                                                  Core.TermLiteral (Core.LiteralString "term"),
                                                  (Core.TermLiteral (Core.LiteralString " in record"))])}))}))))})),
                                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "fieldTerm"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.phantoms.tTerm")),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fieldTerm"))}))})))})),
                                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                                            Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "term"))}))})),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))}))}))}))})))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                                      Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.util.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                          Core.applicationArgument = (Core.TermList [
                                            Core.TermLiteral (Core.LiteralString "missing field "),
                                            Core.TermLiteral (Core.LiteralString "name"),
                                            (Core.TermLiteral (Core.LiteralString " in record"))])}))}))))})),
                                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "fieldTerm"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.core.name")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "fieldTerm"))}))})))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                                      Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "name"))}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "fieldMap"))}))}))}))}))})))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stripped"))}))})))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lexical.stripAndDereferenceTermEither")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))}))),
      Core.bindingType = (Just (Core.TypeScheme {
        Core.typeSchemeVariables = [
          Core.Name "a"],
        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))})),
          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TBinding")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}))}))}))})),
        Core.typeSchemeConstraints = Nothing}))},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.decode.phantoms.tTerm"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "a"),
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected wrapped type hydra.phantoms.TTerm"))}))))),
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
                                    Core.wrappedTermTypeName = (Core.Name "hydra.phantoms.TTerm"),
                                    Core.wrappedTermBody = (Core.TermVariable (Core.Name "b"))}))})))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.decode.core.term")),
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
                Core.applicationArgument = (Core.TermVariable (Core.Name "raw"))}))}))})))})))}))),
      Core.bindingType = (Just (Core.TypeScheme {
        Core.typeSchemeVariables = [
          Core.Name "a"],
        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
          Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "a"))}))}))})),
          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.util.DecodingError")),
                Core.eitherTypeRight = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.phantoms.TTerm")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}))}))}))})),
        Core.typeSchemeConstraints = Nothing}))}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.lexical",
    Module.Namespace "hydra.rewriting",
    (Module.Namespace "hydra.decode.core")],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.phantoms",
    (Module.Namespace "hydra.util")],
  Module.moduleDescription = (Just "Term decoders for hydra.phantoms")}
