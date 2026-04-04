-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.decode.classes

module Hydra.Sources.Decode.Classes where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

module_ :: Packaging.Module
module_ =
    Packaging.Module {
      Packaging.moduleNamespace = (Packaging.Namespace "hydra.decode.classes"),
      Packaging.moduleDefinitions = [
        Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.Name "hydra.decode.classes.typeClass"),
          Packaging.termDefinitionTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                        Core.wrappedTermBody = (Core.TermVariable (Core.Name "err"))}))))})))})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "stripped"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "hydra.core.Term"),
                        Core.caseStatementDefault = (Just (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                          Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expected union"))}))))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "union"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "inj"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLet (Core.Let {
                                Core.letBindings = [
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
                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "equality"))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "input"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                                              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                Core.lambdaParameter = (Core.Name "t"),
                                                Core.lambdaDomain = Nothing,
                                                Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "equality"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeUnit")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))),
                                        (Core.TermPair (Core.TermWrap (Core.WrappedTerm {
                                          Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                          Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "ordering"))}), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "input"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                                              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                                Core.lambdaParameter = (Core.Name "t"),
                                                Core.lambdaDomain = Nothing,
                                                Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                                  Core.injectionTypeName = (Core.Name "hydra.classes.TypeClass"),
                                                  Core.injectionField = Core.Field {
                                                    Core.fieldName = (Core.Name "ordering"),
                                                    Core.fieldTerm = (Core.TermVariable (Core.Name "t"))}}))})))})),
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.extract.core.decodeUnit")),
                                                Core.applicationArgument = (Core.TermVariable (Core.Name "cx"))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "input"))}))}))})))))])})),
                                    Core.bindingType = Nothing}],
                                Core.letBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.maybe"))),
                                      Core.applicationArgument = (Core.TermEither (Left (Core.TermWrap (Core.WrappedTerm {
                                        Core.wrappedTermTypeName = (Core.Name "hydra.errors.DecodingError"),
                                        Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                          Core.applicationArgument = (Core.TermList [
                                            Core.TermLiteral (Core.LiteralString "no such field "),
                                            (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.core.Name")))),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "fname"))})),
                                            (Core.TermLiteral (Core.LiteralString " in union"))])}))}))))})),
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
          Packaging.termDefinitionType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.graph.Graph")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Term")),
                Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "hydra.errors.DecodingError")),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "hydra.classes.TypeClass"))}))}))})),
            Core.typeSchemeConstraints = Nothing}))})],
      Packaging.moduleTermDependencies = [
        Packaging.Namespace "hydra.extract.core",
        (Packaging.Namespace "hydra.lexical"),
        (Packaging.Namespace "hydra.rewriting"),
        (Packaging.Namespace "hydra.decode.core")],
      Packaging.moduleTypeDependencies = [
        Packaging.Namespace "hydra.classes",
        (Packaging.Namespace "hydra.util")],
      Packaging.moduleDescription = (Just "Term decoders for hydra.classes")}
