-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for eta expansion of terms

module Hydra.Test.EtaExpansion where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for eta expansion of terms
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "eta expansion",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Partial application of primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Bare primitives are not expanded",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unary primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "binary primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Partially applied primitives expand with lambdas",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "binary primitive with one argument",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ternary primitive with one argument",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v2"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v2"))}))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Fully applied primitives are not expanded",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unary primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "binary primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a,b,c"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a,b,c"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Record projections",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Bare projections expand with a lambda",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection without argument",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = TestTypes.testTypePersonName,
                      Core.projectionField = (Core.Name "firstName")})))),
                    Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = TestTypes.testTypePersonName,
                          Core.projectionField = (Core.Name "firstName")})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Applied projections are not expanded",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection with argument",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "person"))})),
                    Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection applied to a record",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "John"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))}]}))})),
                    Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "John"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))}]}))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Projections nested in other structures",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection in a list",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermList [
                      Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")}))),
                      (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))]),
                    Testing.etaExpansionTestCaseOutput = (Core.TermList [
                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypePersonName,
                            Core.projectionField = (Core.Name "firstName")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})),
                      (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))])})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection in a tuple",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermProduct [
                      Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")}))),
                      (Core.TermLiteral (Core.LiteralString "default"))]),
                    Testing.etaExpansionTestCaseOutput = (Core.TermProduct [
                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypePersonName,
                            Core.projectionField = (Core.Name "firstName")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})),
                      (Core.TermLiteral (Core.LiteralString "default"))])})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection in let binding",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "getter"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypePersonName,
                            Core.projectionField = (Core.Name "firstName")})))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "getter"))})),
                    Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "getter"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v1"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = TestTypes.testTypePersonName,
                                Core.projectionField = (Core.Name "firstName")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "getter"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection in lambda body",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")}))))}))),
                    Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypePersonName,
                            Core.projectionField = (Core.Name "firstName")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Function-valued projections",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "projection of function-valued field applied to arguments should not be expanded",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                    Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = TestTypes.testTypeTripleName,
                                Core.projectionField = (Core.Name "first")})))),
                              Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                        Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = TestTypes.testTypeTripleName,
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "first"),
                                    Core.fieldTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "second"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "middle"))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "third"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "last"))}]})),
                              Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "DATA"))})),
                    Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = TestTypes.testTypeTripleName,
                                Core.projectionField = (Core.Name "first")})))),
                              Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                        Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                                Core.recordTypeName = TestTypes.testTypeTripleName,
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "first"),
                                    Core.fieldTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "second"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "middle"))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "third"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "last"))}]})),
                              Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "DATA"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic terms (System F)",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Type lambdas in let bindings",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "polymorphic identity function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "id"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "id"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "monomorphic partially applied primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "partial"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "partial"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "partial"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "partial"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "monomorphic projection",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "getter"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "getter"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "getter"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypePersonName,
                            Core.projectionField = (Core.Name "firstName")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "getter"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Type applications of polymorphic bindings",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "polymorphic variable with type application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                    Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type application of identity applied to binary function with no arguments",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                      Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                      Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type application of identity applied to partially applied binary function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                      Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))}))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                      Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type application of identity applied to fully applied binary function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                      Core.typeApplicationTermType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo,bar"))}))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                      Core.typeApplicationTermType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo,bar"))}))}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type application of identity applied to binary function, then applied to one argument",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                        Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))})),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                            Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))})),
                          Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type application of identity applied to binary function, then fully applied to two arguments",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                          Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo,bar"))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "id"),
                      Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "a"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [
                          Core.Name "a"],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "id")),
                          Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo,bar"))}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Higher-Order Functions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Functions that return functions",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning bare binary primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning bare unary primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning partially applied primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))}))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning fully applied primitive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning bare projection",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "person"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = TestTypes.testTypePersonName,
                    Core.projectionField = (Core.Name "firstName")}))))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "person"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lambdas with partial application in body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning lambda returning partial application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})))})))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Let terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "partial application of a let-bound function",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "simple",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "helper"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "arg1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "arg2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "arg3"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "arg1"),
                                Core.TermVariable (Core.Name "arg2"),
                                (Core.TermVariable (Core.Name "arg3"))])}))})))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "helper")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "helper"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "arg1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "arg2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "arg3"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "arg1"),
                                Core.TermVariable (Core.Name "arg2"),
                                (Core.TermVariable (Core.Name "arg3"))])}))})))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v2"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "helper")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "v2"))}))})))})))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "in a fold",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "helper"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "arg1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "arg2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "arg3"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "arg1"),
                                Core.TermVariable (Core.Name "arg2"),
                                (Core.TermVariable (Core.Name "arg3"))])}))})))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "helper")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "bar"),
                      (Core.TermLiteral (Core.LiteralString "baz"))])}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "helper"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "arg1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "arg2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "arg3"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "arg1"),
                                Core.TermVariable (Core.Name "arg2"),
                                (Core.TermVariable (Core.Name "arg3"))])}))})))})))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}))}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v1"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v2"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "helper")),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v2"))}))})))})))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "bar"),
                      (Core.TermLiteral (Core.LiteralString "baz"))])}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "within another let binding",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "tryme"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "helper"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "arg1"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "arg2"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "arg3"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                    Core.applicationArgument = (Core.TermList [
                                      Core.TermVariable (Core.Name "arg1"),
                                      Core.TermVariable (Core.Name "arg2"),
                                      (Core.TermVariable (Core.Name "arg3"))])}))})))})))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                    Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "helper")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}],
                  Core.letBody = Core.TermUnit})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "tryme"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "helper"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "arg1"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "arg2"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "arg3"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                                    Core.applicationArgument = (Core.TermList [
                                      Core.TermVariable (Core.Name "arg1"),
                                      Core.TermVariable (Core.Name "arg2"),
                                      (Core.TermVariable (Core.Name "arg3"))])}))})))})))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                                    Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}))}],
                        Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v1"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v2"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "helper")),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v2"))}))})))})))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))}],
                  Core.letBody = Core.TermUnit}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Case statements",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "monomorphic at top level",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-applied case statement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "string"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "s"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                      Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "string"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "s"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied case statement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                    Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "string"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                          Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                  Core.applicationArgument = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypeUnionMonomorphicName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "string"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                    Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "string"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                          Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                  Core.applicationArgument = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypeUnionMonomorphicName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "string"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied case statement in lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = (Just (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName)),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                      Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "string"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "s"),
                            Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                            Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = (Just (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName)),
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                      Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "string"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "s"),
                            Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                            Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "monomorphic in let binding",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-applied case statement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                        Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "string"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "s"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "ignored"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                            Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "string"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "s"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "ignored"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied case statement",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                          Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "string"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "s"),
                                Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                        Core.applicationArgument = (Core.TermUnion (Core.Injection {
                          Core.injectionTypeName = TestTypes.testTypeUnionMonomorphicName,
                          Core.injectionField = Core.Field {
                            Core.fieldName = (Core.Name "string"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}}))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "ignored"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                          Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "string"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "s"),
                                Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                        Core.applicationArgument = (Core.TermUnion (Core.Injection {
                          Core.injectionTypeName = TestTypes.testTypeUnionMonomorphicName,
                          Core.injectionField = Core.Field {
                            Core.fieldName = (Core.Name "string"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}}))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "ignored"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied case statement in lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName)),
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                            Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "string"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "s"),
                                  Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "ignored"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName)),
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                            Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "string"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "s"),
                                  Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralString "ignored"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "polymorphic in let binding",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-applied UnionPolymorphicRecursive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                        Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                          Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "value"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "i"),
                                Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                        Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                            Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "test"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v1"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "value"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "i"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                            Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "test"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied UnionPolymorphicRecursive with int32",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                            Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "value"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "i"),
                                  Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                          Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                        Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "value"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})),
                          Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "test"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                            Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "value"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "i"),
                                  Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                          Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                        Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "value"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})),
                          Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "test"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied UnionPolymorphicRecursive with int32 in lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                          Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "value"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "i"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                            Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "test"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "test"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                          Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "value"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "i"),
                                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                            Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermVariable (Core.Name "test"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "applied generic UnionPolymorphicRecursive in lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "t0"),
                  Core.typeLambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "test"),
                        Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                          Core.typeLambdaParameter = (Core.Name "t1"),
                          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = (Just (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                  Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                                  Core.caseStatementCases = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "value"),
                                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "ignored"),
                                        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))}]})))),
                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t1"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                    Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "test")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))}))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "t0"),
                  Core.typeLambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "test"),
                        Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                          Core.typeLambdaParameter = (Core.Name "t1"),
                          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = (Just (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))),
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                  Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                                  Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                                  Core.caseStatementCases = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "value"),
                                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "ignored"),
                                        Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))}]})))),
                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t1"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                    Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "test")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))}))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Forced expansion in case statement branches",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable reference in case branch is expanded",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "bool"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "ignored"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "boolean value"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "string"),
                        Core.fieldTerm = (Core.TermVariable (Core.Name "handler"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "unit"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "ignored"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "unit value"))})))}]}))))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                      Core.bindingType = (Just (Core.TypeScheme {
                        Core.typeSchemeVariables = [],
                        Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
                  Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "bool"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "ignored"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "boolean value"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "string"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "v1"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "handler")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "unit"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "ignored"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "unit value"))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare primitive in case branch is expanded",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "bool"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "ignored"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "boolean value"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "string"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))},
                    Core.Field {
                      Core.fieldName = (Core.Name "unit"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "ignored"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "unit value"))})))}]})))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "bool"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "ignored"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "boolean value"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "string"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v1"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "unit"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "ignored"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "unit value"))})))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable reference outside case branch is not expanded",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "handler"))})),
                Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "handler"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "handler"))}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare primitive outside case branch is not expanded",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
                Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Non-expansion of eliminations which produce functions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "applied case statement",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "t0"),
              Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                Core.typeLambdaParameter = (Core.Name "t1"),
                Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "dir"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.coders.CoderDirection"))),
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "coder"),
                    Core.lambdaDomain = (Just (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.Coder")),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))),
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "hydra.coders.CoderDirection"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "encode"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "_"),
                              Core.lambdaDomain = (Just Core.TypeUnit),
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "v12"),
                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.compute.Coder"),
                                              Core.projectionField = (Core.Name "encode")})))),
                                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "coder"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v12"))}))})))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "decode"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "_"),
                              Core.lambdaDomain = (Just Core.TypeUnit),
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "v12"),
                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.compute.Coder"),
                                              Core.projectionField = (Core.Name "decode")})))),
                                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "coder"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v12"))}))})))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "dir"))}))})))})))}))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "t0"),
              Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                Core.typeLambdaParameter = (Core.Name "t1"),
                Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "dir"),
                  Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "hydra.coders.CoderDirection"))),
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "coder"),
                    Core.lambdaDomain = (Just (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.Coder")),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))),
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "hydra.coders.CoderDirection"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "encode"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "_"),
                              Core.lambdaDomain = (Just Core.TypeUnit),
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "v12"),
                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.compute.Coder"),
                                              Core.projectionField = (Core.Name "encode")})))),
                                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "coder"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v12"))}))})))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "decode"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "_"),
                              Core.lambdaDomain = (Just Core.TypeUnit),
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "v12"),
                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                            Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.compute.Coder"),
                                              Core.projectionField = (Core.Name "decode")})))),
                                            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "coder"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v12"))}))})))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "dir"))}))})))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "applied projection",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = TestTypes.testTypeTripleName,
                      Core.projectionField = (Core.Name "third")})))),
                    Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeTripleName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "first"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "second"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "third"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "s"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))}]}))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = TestTypes.testTypeTripleName,
                      Core.projectionField = (Core.Name "third")})))),
                    Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.typeApplicationTermType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeTripleName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "first"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "second"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "third"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "s"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))}]}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
