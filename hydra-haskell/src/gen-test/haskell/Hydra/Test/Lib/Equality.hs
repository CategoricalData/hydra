-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.equality primitives

module Hydra.Test.Lib.Equality where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for hydra.lib.equality primitives
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "hydra.lib.equality primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "compare",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "greaterThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "equal",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unequal integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "gt",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "gte",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gte"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gte"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gte"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "identity",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.identity"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "lt",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "lte",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lte"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lte"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lte"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "max",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.max"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "second greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.max"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.max"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "min",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.min"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "second less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.min"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.min"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "compare strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less than (lexicographic)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "banana"))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater than (lexicographic)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "greaterThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty vs non-empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "prefix vs longer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ab"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "lt strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less (lexicographic)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "banana"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "gt strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater (lexicographic)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "banana"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "max strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.max"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "zebra"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "second greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.max"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "zebra"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.max"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "min strings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.min"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "apple"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "second less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.min"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "zebra"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "apple"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "apple"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.min"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "compare floats",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "greaterThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative vs positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-1.0))))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "lt floats",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.lt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "gt floats",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "greater",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 5.0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "less",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.gt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.5)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 2.5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
