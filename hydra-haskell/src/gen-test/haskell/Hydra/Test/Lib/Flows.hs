-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.flows primitives

module Hydra.Test.Lib.Flows where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for hydra.lib.flows primitives
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "hydra.lib.flows primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "apply",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "apply add",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "f"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bind",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bind add",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "n"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bind multiply",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "n"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))}))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "fail",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fail with message",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.fail")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "test error message"))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe Nothing)},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralString "Error: test error message ()")])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "foldl",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "foldl sum",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a0"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a0"))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a1"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a1"))})),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))})),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a2"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a2"))})),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))}))})))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "map",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map negate",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.map"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate")))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5))))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map abs",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.map"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.abs")))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-3))))}))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapElems",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mapElems add one",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "v2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermVariable (Core.Name "v1"))),
                                (Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermVariable (Core.Name "v2"))))])}))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermMap (M.fromList [
                    (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))),
                    (Core.TermLiteral (Core.LiteralString "b"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))]))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapKeys",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mapKeys add one",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "k1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "k2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.fromList"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermPair (Core.TermVariable (Core.Name "k1"), (Core.TermLiteral (Core.LiteralString "a"))),
                                (Core.TermPair (Core.TermVariable (Core.Name "k2"), (Core.TermLiteral (Core.LiteralString "b"))))])}))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermMap (M.fromList [
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "a"))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)), (Core.TermLiteral (Core.LiteralString "b")))]))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapList",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mapList add one",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y3"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermList [
                                  Core.TermVariable (Core.Name "y1"),
                                  (Core.TermVariable (Core.Name "y2")),
                                  (Core.TermVariable (Core.Name "y3"))])}))})))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapMaybe",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mapMaybe just",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "y"))))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mapMaybe nothing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                    Core.applicationArgument = (Core.TermMaybe Nothing)}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermMaybe Nothing)))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "mapSet",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mapSet add one",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y3"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.fromList"))),
                                  Core.applicationArgument = (Core.TermList [
                                    Core.TermVariable (Core.Name "y1"),
                                    (Core.TermVariable (Core.Name "y2")),
                                    (Core.TermVariable (Core.Name "y3"))])}))}))})))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermSet (S.fromList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "pure",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "pure integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "pure zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "pure negative",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5))))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5))))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "pure string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello"))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "sequence",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "sequence pure list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x1"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x2"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.bind")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))})),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x3"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                                Core.applicationArgument = (Core.TermList [
                                  Core.TermVariable (Core.Name "x1"),
                                  (Core.TermVariable (Core.Name "x2")),
                                  (Core.TermVariable (Core.Name "x3"))])}))})))}))})))}))})))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "withDefault",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "withDefault on success returns original",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.withDefault"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.pure")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "withDefault on failure returns fallback",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.compute.Flow")))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.withDefault"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.fail")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "error"))}))}))})),
                Core.applicationArgument = Core.TermUnit})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "stack"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "messages"),
                    Core.fieldTerm = (Core.TermList [])},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermMap M.empty)}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "hydra.compute.FlowState"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))))},
                Core.Field {
                  Core.fieldName = (Core.Name "state"),
                  Core.fieldTerm = Core.TermUnit},
                Core.Field {
                  Core.fieldName = (Core.Name "trace"),
                  Core.fieldTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "hydra.compute.Trace"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "stack"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "messages"),
                        Core.fieldTerm = (Core.TermList [])},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermMap M.empty)}]}))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
