-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.literals primitives

module Hydra.Test.Lib.Literals where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Hydra.Lib.Literals as Literals

-- | Test cases for hydra.lib.literals primitives
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "hydra.lib.literals primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "bigintToInt8",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-42))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToInt16",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-1000))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (-1000))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToInt32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-42))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToInt64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-1000000))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (-1000000))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToUint8",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 100)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 100)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToUint16",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 1000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToUint32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 100000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 100000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToUint64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1000000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "int8ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int8ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int8ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "max value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int8ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 127)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 127)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "min value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int8ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-128))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-128))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "int16ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int16ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int16ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (-1000))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-1000))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "int32ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int32ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int32ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int32ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "int64ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int64ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.int64ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (-1000000))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-1000000))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "uint8ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint8ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "max value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint8ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 255)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 255)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "uint16ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint16ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint16ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 1000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "uint32ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint32ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint32ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 100000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 100000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "uint64ToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint64ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.uint64ToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1000000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 1000000)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "float32ToBigfloat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.float32ToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 2.5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.float32ToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-2.5))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-2.5))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.float32ToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "float64ToBigfloat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.float64ToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14159)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.float64ToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.71828))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-2.71828))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.float64ToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigfloatToFloat32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-2.5))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-2.5))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 0.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigfloatToFloat64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14159)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-2.71828))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.71828))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigintToBigfloat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 42.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-42.0))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigintToBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bigfloatToBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 42.7)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 43)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat (-42.7))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-43))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "round down",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 42.3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "half even up",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 42.5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "half even down",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.bigfloatToBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 43.5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 44)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showInt8",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "42"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "-42"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showInt16",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "1000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (-1000))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "-1000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showInt32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "42"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "-42"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showInt64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "1000000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (-1000000))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "-1000000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showUint8",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "max value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 255)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "255"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showUint16",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 1000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "1000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showUint32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 100000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "100000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showUint64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1000000)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "1000000"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "42"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "-42"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showFloat32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "3.14"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-2.5))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "-2.5"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0.0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showFloat64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "3.14159"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0.0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showBigfloat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "3.14"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 0.0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "0.0"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showBoolean",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBoolean"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "true"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showBoolean"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "false"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "showString",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\"hello\""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.showString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\"\""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readInt8",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "42"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-42"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-42))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "max value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "127"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 127)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "min value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-128"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 (-128))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "overflow",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "128"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readInt16",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "1000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 1000)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-1000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 (-1000))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readInt32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "42"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-42"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-42))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readInt64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "1000000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 1000000)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-1000000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (-1000000))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readInt64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readUint8",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "0"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 0)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "100"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 100)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "max value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "255"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 255)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint8"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-1"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readUint16",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "0"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 0)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "1000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 1000)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint16"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-1"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readUint32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "0"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 0)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "100000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 100000)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-1"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readUint64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "0"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 0)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "typical",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "1000000"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1000000)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readUint64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-1"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readBigint",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "42"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-42"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint (-42))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "0"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 0)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "large",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "123456789012345678901234567890"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 123456789012345678901234567890)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigint"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readFloat32",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "3.14"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-2.5"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-2.5))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readFloat32"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readFloat64",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "3.14159"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14159)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "-2.71828"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.71828))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readFloat64"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readBigfloat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "3.14"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBigfloat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readBoolean",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBoolean"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "true"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralBoolean True))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBoolean"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "false"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralBoolean False))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readBoolean"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "yes"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "readString",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "quoted string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\"hello\""))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty quoted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\"\""))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString ""))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unquoted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.readString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "stringToBinary",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple base64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.stringToBinary"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "aGVsbG8="))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "aGVsbG8=")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.stringToBinary"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "binaryToString",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple binary",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.binaryToString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "aGVsbG8=")))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "aGVsbG8="))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty binary",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.literals.binaryToString"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBinary (Literals.stringToBinary "")))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
