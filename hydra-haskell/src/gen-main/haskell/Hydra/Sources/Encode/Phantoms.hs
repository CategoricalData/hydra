-- Note: this is an automatically generated file. Do not edit.

-- | Source module for hydra.encode.phantoms

module Hydra.Sources.Encode.Phantoms where

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
  Module.moduleNamespace = (Module.Namespace "hydra.encode.phantoms"),
  Module.moduleElements = [
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.phantoms.tBinding"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "a"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.phantoms.TBinding"))}))},
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
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.phantoms.TBinding"),
                                  Core.projectionField = (Core.Name "name")})))),
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
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.phantoms.tTerm")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.phantoms.TBinding"),
                                  Core.projectionField = (Core.Name "term")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))])}]}))}}))})))}))),
      Core.bindingType = Nothing},
    Core.Binding {
      Core.bindingName = (Core.Name "hydra.encode.phantoms.tTerm"),
      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
        Core.lambdaParameter = (Core.Name "a"),
        Core.lambdaDomain = Nothing,
        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermUnion (Core.Injection {
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
                      Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.phantoms.TTerm"))}))},
                  Core.Field {
                    Core.fieldName = (Core.Name "body"),
                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.encode.core.term")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.phantoms.TTerm")))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}]}))}}))})))}))),
      Core.bindingType = Nothing}],
  Module.moduleTermDependencies = [
    Module.Namespace "hydra.encode.core"],
  Module.moduleTypeDependencies = [
    Module.Namespace "hydra.phantoms"],
  Module.moduleDescription = (Just "Term encoders for hydra.phantoms")}
