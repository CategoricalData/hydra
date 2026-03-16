-- Note: this is an automatically generated file. Do not edit.

-- | Inference tests for nominal types

module Hydra.Test.Inference.NominalTypes where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTerms as TestTerms
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Nominal type tests
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Nominal terms",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    testGroupForCaseStatements,
    testGroupForProjections,
    testGroupForRecords,
    testGroupForVariants,
    testGroupForWrappers],
  Testing.testGroupCases = []}

testGroupForCaseStatements :: Testing.TestGroup
testGroupForCaseStatements = Testing.TestGroup {
  Testing.testGroupName = "Case statements",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#1",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
          Core.caseStatementTypeName = TestTypes.testTypeSimpleNumberName,
          Core.caseStatementDefault = Nothing,
          Core.caseStatementCases = [
            Core.Field {
              Core.fieldName = (Core.Name "int"),
              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
            Core.Field {
              Core.fieldName = (Core.Name "float"),
              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))}]})))),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeSimpleNumberName),
            Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = [
        Testing.Tag "disabledForMinimalInference"]},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#2",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
          Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
          Core.caseStatementDefault = Nothing,
          Core.caseStatementCases = [
            Core.Field {
              Core.fieldName = (Core.Name "bool"),
              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean True))})))},
            Core.Field {
              Core.fieldName = (Core.Name "string"),
              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))},
            Core.Field {
              Core.fieldName = (Core.Name "unit"),
              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))}]})))),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName),
            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = [
        Testing.Tag "disabledForMinimalInference"]}]}

testGroupForProjections :: Testing.TestGroup
testGroupForProjections = Testing.TestGroup {
  Testing.testGroupName = "Projections",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Record eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypePersonName,
              Core.projectionField = (Core.Name "firstName")})))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypePersonName),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]},
    Testing.TestGroup {
      Testing.testGroupName = "Pair projections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0",
                (Core.Name "t1")],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.second"))),
              Core.applicationArgument = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "foo"))))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}],
  Testing.testGroupCases = []}

testGroupForRecords :: Testing.TestGroup
testGroupForRecords = Testing.TestGroup {
  Testing.testGroupName = "Records",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Simple records",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypeLatLonName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 37.7749)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-122.4194))))}]})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypeLatLonName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 37.7749)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-122.4194))))}]})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeLatLonPolyName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "lon"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 37.7749)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "lon"))}]}))}))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeLatLonPolyName),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "latlon"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "latlon"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "latlon"))}]}))}))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeLatLonPolyName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#5",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = TestTerms.testDataArthur,
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypePersonName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]},
    Testing.TestGroup {
      Testing.testGroupName = "Record instances of simply recursive record types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypeIntListName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "head"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                Core.Field {
                  Core.fieldName = (Core.Name "tail"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeIntListName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "head"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "tail"),
                        Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypeIntListName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeIntListName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "head"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "tail"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypeIntListName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "head"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "tail"),
                            Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]}))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypeIntListName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypeListName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "head"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                Core.Field {
                  Core.fieldName = (Core.Name "tail"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeListName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "head"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "tail"),
                        Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeListName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeListName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "head"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "tail"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypeListName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "head"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "tail"),
                            Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]}))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeListName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#5",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeListName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "head"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "tail"),
                    Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypeListName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "head"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "tail"),
                          Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]}))}))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeListName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]},
    Testing.TestGroup {
      Testing.testGroupName = "Record instances of mutually recursive record types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeBuddyListAName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "head"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "tail"),
                      Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypeBuddyListBName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "head"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "tail"),
                            Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]}))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeBuddyListAName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "head"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "tail"),
                    Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypeBuddyListBName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "head"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "tail"),
                          Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]}))}))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}],
  Testing.testGroupCases = []}

testGroupForVariants :: Testing.TestGroup
testGroupForVariants = Testing.TestGroup {
  Testing.testGroupName = "Variant terms",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Variants",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeTimestampName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "unixTimeMillis"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1638200308368)))}})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypeTimestampName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeUnionMonomorphicName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "string"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]},
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic and recursive variants",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "bool"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean True))}})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [
                Core.Name "t0"],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "value"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "other"),
                  Core.bindingTerm = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "other"),
                  Core.fieldTerm = (Core.TermVariable (Core.Name "other"))}}))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}],
  Testing.testGroupCases = []}

testGroupForWrappers :: Testing.TestGroup
testGroupForWrappers = Testing.TestGroup {
  Testing.testGroupName = "Wrapper introductions and eliminations",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Wrapper introductions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeVariable TestTypes.testTypeStringAliasName),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                Core.wrappedTermBody = (Core.TermVariable (Core.Name "v"))}))}))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                Core.functionTypeCodomain = (Core.TypeVariable TestTypes.testTypeStringAliasName)})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]},
    Testing.TestGroup {
      Testing.testGroupName = "Wrapper eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeStringAliasName),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
            Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
            Testing.inferenceTestCaseOutput = Core.TypeScheme {
              Core.typeSchemeVariables = [],
              Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
              Core.typeSchemeConstraints = Nothing}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForMinimalInference"]}]}],
  Testing.testGroupCases = []}
