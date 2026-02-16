-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.eithers primitives

module Hydra.Test.Lib.Eithers where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for hydra.lib.eithers primitives
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "hydra.lib.eithers primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "bind",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bind Right with success",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "ab"))))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "s"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})),
                    Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))))})),
                  Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bind Right with failure",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString ""))))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "s"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})),
                    Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))))})),
                  Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bind Left returns Left unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bind"))),
                Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "s"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})),
                    Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))))})),
                  Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bimap",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map left value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bimap"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))})),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "s"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))})),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map right value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.bimap"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))})),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "s"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))})),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "ab"))))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "isLeft",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "left value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.isLeft"))),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "right value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.isLeft"))),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "test"))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "isRight",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "right value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.isRight"))),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "test"))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "left value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.isRight"))),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "fromLeft",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "extract left",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.fromLeft"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))})),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "use default for right",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.fromLeft"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))})),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "test"))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "fromRight",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "extract right",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.fromRight"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "default"))})),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "test"))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "test"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "use default for left",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.fromRight"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "default"))})),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "default"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "either",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "apply left function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))})),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "s"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))})),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "apply right function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.either"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))})),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "s"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))})),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "ab"))))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "lefts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "filter left values",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.lefts"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "a")))),
                (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "b"))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all lefts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.lefts"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all rights",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.lefts"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "a"))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "b"))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.lefts"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "rights",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "filter right values",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.rights"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "a")))),
                (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "b"))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all rights",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.rights"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "a"))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "b"))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all lefts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.rights"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.rights"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "partitionEithers",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "partition mixed",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.partitionEithers"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "a")))),
                (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "b"))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermPair (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all lefts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.partitionEithers"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
                (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermPair (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermList [])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all rights",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.partitionEithers"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "a"))),
                (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "b"))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermPair (Core.TermList [], (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.partitionEithers"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermPair (Core.TermList [], (Core.TermList [])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "map",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map right value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))})),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "preserve left",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))})),
              Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapList",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all succeed",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.mapList"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                      Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
                    Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))}))})))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first fails",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.mapList"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                      Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
                    Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))}))})))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.mapList"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                      Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
                    Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))}))})))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermList [])))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapMaybe",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "just succeeds",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.mapMaybe"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                      Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
                    Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))}))})))})),
              Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "just fails",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.mapMaybe"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                      Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
                    Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))}))})))})),
              Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))))})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.eithers.mapMaybe"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                      Core.applicationArgument = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "zero"))))})),
                    Core.applicationArgument = (Core.TermEither (Right (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))))}))})))})),
              Core.applicationArgument = (Core.TermMaybe Nothing)})),
            Testing.evaluationTestCaseOutput = (Core.TermEither (Right (Core.TermMaybe Nothing)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
