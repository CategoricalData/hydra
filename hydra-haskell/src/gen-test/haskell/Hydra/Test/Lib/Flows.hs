-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.flows primitives

module Hydra.Test.Lib.Flows where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
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
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.map")),
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
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.monads.map")),
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
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
