-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for term reduction/evaluation mechanics

module Hydra.Test.Reduction where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for term reduction mechanics
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "reduction",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "beta reduction",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity function applied to literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "constant function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "monomorphic primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "toUpper on lowercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "HELLO"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "toUpper on mixed case",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Hello World"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "HELLO WORLD"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "toUpper on empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "toLower on uppercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "HELLO"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string length",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string length of empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "add two positive integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "add negative and positive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-10))))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-7))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "add with zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "subtract integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiply integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiply by zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "divide integers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.div"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "modulo",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mod"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 17)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "splitOn basic",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a,b,c"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString "b"),
              (Core.TermLiteral (Core.LiteralString "c"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cat2 strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "helloworld"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "polymorphic primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "length of integer list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "length of string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "length of empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "length of single element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralBoolean True)])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "head of integer list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "head of string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "first"),
                (Core.TermLiteral (Core.LiteralString "second"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "first"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "last of integer list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.last"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "concat two integer lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "concat with empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "reverse integer list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "reverse empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "nullary primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty set has size zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.size"))),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.empty")))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "literals as values",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "integer literal is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative integer literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17)))),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero integer literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string literal is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralString "hello")),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralString "")),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with special characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralString "hello\nworld\ttab")),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello\nworld\ttab"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean true is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralBoolean True)),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean false is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralBoolean False)),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float literal is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14))),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative float literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.718)))),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.718))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero float literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "list reduction",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermList []),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of literals is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list with reducible element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermList [
              Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "optional reduction",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nothing is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermMaybe Nothing),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "just literal is a value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "just with reducible content",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermMaybe (Just (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
            Testing.evaluationTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "alpha conversion",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermVariable (Core.Name "x")),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermVariable (Core.Name "y"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              (Core.TermVariable (Core.Name "x"))]),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              (Core.TermVariable (Core.Name "y"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with different variable is transparent",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "z"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                Core.TermVariable (Core.Name "x"),
                (Core.TermVariable (Core.Name "z"))])}))),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "z"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                Core.TermVariable (Core.Name "y"),
                (Core.TermVariable (Core.Name "z"))])})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with same variable is opaque",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                Core.TermVariable (Core.Name "x"),
                (Core.TermVariable (Core.Name "z"))])}))),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                Core.TermVariable (Core.Name "x"),
                (Core.TermVariable (Core.Name "z"))])})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested lambda outer variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "b"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "a"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "b"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested lambda shadows outer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "z"),
            Testing.alphaConversionTestCaseResult = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application with variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application with both variables same",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseAlphaConversion (Testing.AlphaConversionTestCase {
            Testing.alphaConversionTestCaseTerm = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
            Testing.alphaConversionTestCaseOldVariable = (Core.Name "x"),
            Testing.alphaConversionTestCaseNewVariable = (Core.Name "y"),
            Testing.alphaConversionTestCaseResult = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "type reduction",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unit type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = Core.TypeUnit,
            Testing.typeReductionTestCaseOutput = Core.TypeUnit})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.typeReductionTestCaseOutput = (Core.TypeLiteral Core.LiteralTypeString)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.typeReductionTestCaseOutput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity type applied to string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))}))})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.typeReductionTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "constant type ignores argument",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "x"),
                Core.forallTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
            Testing.typeReductionTestCaseOutput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested forall first application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "x"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "y"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))}))})),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.typeReductionTestCaseOutput = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "y"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested forall both applications",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "x"),
                  Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "y"),
                    Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))}))})),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.typeReductionTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list type applied",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "a"),
                Core.forallTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "a")))})),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.typeReductionTestCaseOutput = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optional type applied",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeReduction (Testing.TypeReductionTestCase {
            Testing.typeReductionTestCaseInput = (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "a"),
                Core.forallTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "a")))})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.typeReductionTestCaseOutput = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "hoistSubterms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistNothing: simple let unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateNothing,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistNothing: let with list in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateNothing,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistNothing: let with application in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateNothing,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "g")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "g")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: list in body is hoisted into local let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: multiple lists in body are hoisted together",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_2"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_2"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: list in binding value is hoisted into local let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: nested lists hoisted from inside out",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_2"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_1"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_2"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: application in list element is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}),
                (Core.TermVariable (Core.Name "y"))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "_hoist_1"),
                  (Core.TermVariable (Core.Name "y"))])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: application in record field is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "Data"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "value"),
                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Data"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "_hoist_1"))}]}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: nested applications hoisted from inside out",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_2"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "_hoist_2")])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistCaseStatements: case in application argument is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateCaseStatements,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistCaseStatements: case in list element is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateCaseStatements,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "ok"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "err"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Result"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "ok"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "err"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "_hoist_1")])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: nested let - inner let processed independently",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermVariable (Core.Name "y"))])}))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "_hoist_1"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: non-let term is unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: bare application unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: term referring to let-bound variable needs no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: term referring to lambda above let needs no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "y"),
                    (Core.TermVariable (Core.Name "x"))])}))}))}))),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "_hoist_1"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "y"),
                        (Core.TermVariable (Core.Name "x"))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: lambda-bound var not free in hoisted term needs no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))})))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: lambda-bound var free in hoisted term requires capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermVariable (Core.Name "y"))])}))})))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))])}))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: only free lambda-bound vars are captured",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "a"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "b"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "b"))])}))})))})))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "b"))])}))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "a"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))})))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: counter threads through binding and body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_2"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_2"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: counter threads through multiple bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_2"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_2"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: polymorphic binding with self-reference below hoisted term",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermList [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "hoistCaseStatements",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case at top level of let body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "Optional"),
                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "just"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "nothing"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "Optional"),
                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "just"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "nothing"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in let binding value is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside lambda body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside nested lambdas is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "b"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "b"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case as LHS of one application is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case wrapped in annotation is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.annotatedTermAnnotation = M.empty})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.annotatedTermAnnotation = M.empty})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in lambda with one application is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case as RHS of application IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in nested application LHS IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "w"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "w"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "z"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "w"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "w"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside list element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_1")])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside lambda inside list IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list inside lambda is NOT hoisted (only case statements)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "a"),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "a"),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in binding is not hoisted, case in arg position is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "w"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "w"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in nested let body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "z"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "w"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "z"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "w"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in let inside lambda is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in lambda inside let body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "a"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "a"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case with let+lambda+app is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in triple application LHS IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "c"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))})))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "c"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))})))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "z"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "c"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))})))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "c"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))})))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case as second argument IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in both arguments - both hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_2"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in second list element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermVariable (Core.Name "_hoist_1"))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple cases in list - all hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))),
                    (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_1"),
                      (Core.TermVariable (Core.Name "_hoist_2"))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in pair first element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermPair (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "_hoist_1"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in pair second element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermVariable (Core.Name "_hoist_1"))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in child let binding hoisted into child",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "_hoist_1"),
                              Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = Nothing,
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "y"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in child let body hoisted into child",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "_hoist_1"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case at top level of child let NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cases in both outer and child - each hoisted locally",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                    Core.applicationArgument = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "inner"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))))})),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "inner"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_2"))})),
                      Core.applicationArgument = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "inner"),
                            Core.bindingTerm = (Core.TermLet (Core.Let {
                              Core.letBindings = [
                                Core.Binding {
                                  Core.bindingName = (Core.Name "_hoist_1"),
                                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                    Core.caseStatementTypeName = (Core.Name "Optional"),
                                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                                    Core.caseStatementCases = [
                                      Core.Field {
                                        Core.fieldName = (Core.Name "just"),
                                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "y"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                                      Core.Field {
                                        Core.fieldName = (Core.Name "nothing"),
                                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})))),
                                  Core.bindingType = Nothing}],
                              Core.letBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_1"))}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "inner"))}))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda after app LHS takes us out of top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside case branch is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside case default branch is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in arg position inside case branch IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "a"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_1")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
