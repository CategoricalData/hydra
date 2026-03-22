-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.error.core

module Hydra.Sources.Encode.Error.Core where

import qualified Hydra.Core as Core
import qualified Hydra.Module as Module
import Hydra.Module.Compat (bindingToDefinition)
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

module_ :: Module.Module
module_ =
    Module.Module {
      Module.moduleNamespace = (Module.Namespace "hydra.encode.error.core"),
      Module.moduleDefinitions = L.map bindingToDefinition [
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.duplicateBindingError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.DuplicateBindingError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateBindingError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateBindingError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.duplicateFieldError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.DuplicateFieldError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateFieldError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateFieldError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.undefinedFieldError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UndefinedFieldError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "fieldName"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.name")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
                                    Core.projectionField = (Core.Name "fieldName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedFieldError"),
                                    Core.projectionField = (Core.Name "typeName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedFieldError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.unexpectedTermVariantError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UnexpectedTermVariantError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expectedVariant"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.variants.termVariant")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
                                    Core.projectionField = (Core.Name "expectedVariant")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "actualTerm"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTermVariantError"),
                                    Core.projectionField = (Core.Name "actualTerm")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UnexpectedTermVariantError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.unexpectedTypeVariantError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UnexpectedTypeVariantError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "expectedVariant"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.variants.typeVariant")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
                                    Core.projectionField = (Core.Name "expectedVariant")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "actualType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnexpectedTypeVariantError"),
                                    Core.projectionField = (Core.Name "actualType")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UnexpectedTypeVariantError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.constantConditionError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.ConstantConditionError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                                    Core.injectionField = Core.Field {
                                      Core.fieldName = (Core.Name "literal"),
                                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                                        Core.injectionTypeName = (Core.Name "hydra.core.Literal"),
                                        Core.injectionField = Core.Field {
                                          Core.fieldName = (Core.Name "boolean"),
                                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}}))}))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.ConstantConditionError"),
                                    Core.projectionField = (Core.Name "value")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.ConstantConditionError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyCaseStatementError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyCaseStatementError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyCaseStatementError"),
                                    Core.projectionField = (Core.Name "typeName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyCaseStatementError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyLetBindingsError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyLetBindingsError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyLetBindingsError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyLetBindingsError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyTermAnnotationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyTermAnnotationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTermAnnotationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyTermAnnotationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyTypeNameInTermError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyTypeNameInTermError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeNameInTermError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyTypeNameInTermError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidLambdaParameterNameError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidLambdaParameterNameError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLambdaParameterNameError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidLambdaParameterNameError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidLetBindingNameError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidLetBindingNameError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidLetBindingNameError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidLetBindingNameError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidTypeLambdaParameterNameError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeLambdaParameterNameError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTypeLambdaParameterNameError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.nestedTermAnnotationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.NestedTermAnnotationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTermAnnotationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.NestedTermAnnotationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.redundantWrapUnwrapError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.RedundantWrapUnwrapError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.RedundantWrapUnwrapError"),
                                    Core.projectionField = (Core.Name "typeName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.RedundantWrapUnwrapError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.selfApplicationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.SelfApplicationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.SelfApplicationError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.SelfApplicationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.termVariableShadowingError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.TermVariableShadowingError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.TermVariableShadowingError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.TermVariableShadowingError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.typeVariableShadowingInTypeLambdaError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.TypeVariableShadowingInTypeLambdaError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.TypeVariableShadowingInTypeLambdaError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.undefinedTermVariableError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UndefinedTermVariableError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTermVariableError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTermVariableError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.undefinedTypeVariableInBindingTypeError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UndefinedTypeVariableInBindingTypeError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableInBindingTypeError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.undefinedTypeVariableInLambdaDomainError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableInLambdaDomainError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.undefinedTypeVariableInTypeApplicationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableInTypeApplicationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.unknownPrimitiveNameError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UnknownPrimitiveNameError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnknownPrimitiveNameError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UnknownPrimitiveNameError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.unnecessaryIdentityApplicationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UnnecessaryIdentityApplicationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UnnecessaryIdentityApplicationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.untypedTermVariableError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UntypedTermVariableError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UntypedTermVariableError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UntypedTermVariableError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidTermError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.error.core.InvalidTermError"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "constantCondition"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "constantCondition"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.constantConditionError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "duplicateBinding"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateBinding"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.duplicateBindingError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "duplicateField"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateField"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.duplicateFieldError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyCaseStatement"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyCaseStatement"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyCaseStatementError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyLetBindings"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyLetBindings"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyLetBindingsError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyTermAnnotation"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyTermAnnotation"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyTermAnnotationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyTypeNameInTerm"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyTypeNameInTerm"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyTypeNameInTermError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "invalidLambdaParameterName"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidLambdaParameterName"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.invalidLambdaParameterNameError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "invalidLetBindingName"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidLetBindingName"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.invalidLetBindingNameError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "invalidTypeLambdaParameterName"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidTypeLambdaParameterName"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.invalidTypeLambdaParameterNameError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "nestedTermAnnotation"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nestedTermAnnotation"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.nestedTermAnnotationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "redundantWrapUnwrap"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "redundantWrapUnwrap"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.redundantWrapUnwrapError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "selfApplication"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "selfApplication"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.selfApplicationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "termVariableShadowing"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "termVariableShadowing"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.termVariableShadowingError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "typeVariableShadowingInTypeLambda"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeVariableShadowingInTypeLambda"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.typeVariableShadowingInTypeLambdaError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "undefinedTermVariable"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTermVariable"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.undefinedTermVariableError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "undefinedTypeVariableInBindingType"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariableInBindingType"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.undefinedTypeVariableInBindingTypeError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "undefinedTypeVariableInLambdaDomain"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariableInLambdaDomain"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.undefinedTypeVariableInLambdaDomainError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "undefinedTypeVariableInTypeApplication"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariableInTypeApplication"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.undefinedTypeVariableInTypeApplicationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "unknownPrimitiveName"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unknownPrimitiveName"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.unknownPrimitiveNameError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "unnecessaryIdentityApplication"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "unnecessaryIdentityApplication"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.unnecessaryIdentityApplicationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "untypedTermVariable"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTermError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "untypedTermVariable"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.untypedTermVariableError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]})))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTermError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.duplicateRecordTypeFieldNamesError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.DuplicateRecordTypeFieldNamesError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateRecordTypeFieldNamesError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.duplicateUnionTypeFieldNamesError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.DuplicateUnionTypeFieldNamesError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.DuplicateUnionTypeFieldNamesError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyRecordTypeError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyRecordTypeError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyRecordTypeError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyRecordTypeError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyTypeAnnotationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyTypeAnnotationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyTypeAnnotationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyTypeAnnotationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.emptyUnionTypeError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.EmptyUnionTypeError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.EmptyUnionTypeError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.EmptyUnionTypeError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidForallParameterNameError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidForallParameterNameError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidForallParameterNameError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidForallParameterNameError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidTypeSchemeVariableNameError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeSchemeVariableNameError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTypeSchemeVariableNameError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.nestedTypeAnnotationError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.NestedTypeAnnotationError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.NestedTypeAnnotationError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.NestedTypeAnnotationError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.nonComparableMapKeyTypeError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.NonComparableMapKeyTypeError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "keyType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableMapKeyTypeError"),
                                    Core.projectionField = (Core.Name "keyType")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.NonComparableMapKeyTypeError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.nonComparableSetElementTypeError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.NonComparableSetElementTypeError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
                          Core.recordTypeName = (Core.Name "hydra.core.Field"),
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "name"),
                              Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "elementType"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.type")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.NonComparableSetElementTypeError"),
                                    Core.projectionField = (Core.Name "elementType")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.NonComparableSetElementTypeError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.singleVariantUnionError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.SingleVariantUnionError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
                                    Core.projectionField = (Core.Name "location")})))),
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.SingleVariantUnionError"),
                                    Core.projectionField = (Core.Name "fieldName")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.SingleVariantUnionError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.typeVariableShadowingInForallError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.TypeVariableShadowingInForallError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.TypeVariableShadowingInForallError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.TypeVariableShadowingInForallError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.undefinedTypeVariableError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.UndefinedTypeVariableError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}),
                        (Core.TermRecord (Core.Record {
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
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.UndefinedTypeVariableError"),
                                    Core.projectionField = (Core.Name "name")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.UndefinedTypeVariableError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.voidInNonBottomPositionError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                        Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.VoidInNonBottomPositionError"))}))},
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
                                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "location"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "term"),
                              Core.fieldTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.accessors.accessorPath")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                    Core.projectionTypeName = (Core.Name "hydra.error.core.VoidInNonBottomPositionError"),
                                    Core.projectionField = (Core.Name "location")})))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]})])}]}))}}))}))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.VoidInNonBottomPositionError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))},
        Core.Binding {
          Core.bindingName = (Core.Name "hydra.encode.error.core.invalidTypeError"),
          Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
            Core.caseStatementTypeName = (Core.Name "hydra.error.core.InvalidTypeError"),
            Core.caseStatementDefault = Nothing,
            Core.caseStatementCases = [
              Core.Field {
                Core.fieldName = (Core.Name "duplicateRecordTypeFieldNames"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateRecordTypeFieldNames"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.duplicateRecordTypeFieldNamesError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "duplicateUnionTypeFieldNames"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "duplicateUnionTypeFieldNames"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.duplicateUnionTypeFieldNamesError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyRecordType"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyRecordType"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyRecordTypeError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyTypeAnnotation"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyTypeAnnotation"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyTypeAnnotationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "emptyUnionType"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "emptyUnionType"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.emptyUnionTypeError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "invalidForallParameterName"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidForallParameterName"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.invalidForallParameterNameError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "invalidTypeSchemeVariableName"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "invalidTypeSchemeVariableName"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.invalidTypeSchemeVariableNameError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "nestedTypeAnnotation"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nestedTypeAnnotation"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.nestedTypeAnnotationError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "nonComparableMapKeyType"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nonComparableMapKeyType"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.nonComparableMapKeyTypeError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "nonComparableSetElementType"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "nonComparableSetElementType"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.nonComparableSetElementTypeError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "singleVariantUnion"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "singleVariantUnion"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.singleVariantUnionError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "typeVariableShadowingInForall"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "typeVariableShadowingInForall"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.typeVariableShadowingInForallError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "undefinedTypeVariable"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "undefinedTypeVariable"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.undefinedTypeVariableError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))},
              Core.Field {
                Core.fieldName = (Core.Name "voidInNonBottomPosition"),
                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = (Core.Name "hydra.core.Term"),
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "union"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = (Core.Name "hydra.core.Injection"),
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "typeName"),
                            Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                              Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.error.core.InvalidTypeError"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "field"),
                            Core.fieldTerm = (Core.TermRecord (Core.Record {
                              Core.recordTypeName = (Core.Name "hydra.core.Field"),
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "name"),
                                  Core.fieldTerm = (Core.TermWrap (Core.WrappedTerm {
                                    Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "voidInNonBottomPosition"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "term"),
                                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.error.core.voidInNonBottomPositionError")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}]}))}]}))}}))})))}]})))),
          Core.bindingType = (Just (Core.TypeScheme {
            Core.typeSchemeVariables = [],
            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.error.core.InvalidTypeError")),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "hydra.core.Term"))})),
            Core.typeSchemeConstraints = Nothing}))}],
      Module.moduleTermDependencies = [
        Module.Namespace "hydra.encode.accessors",
        (Module.Namespace "hydra.encode.core"),
        (Module.Namespace "hydra.encode.variants")],
      Module.moduleTypeDependencies = [
        Module.Namespace "hydra.error.core"],
      Module.moduleDescription = (Just "Term encoders for hydra.error.core")}
