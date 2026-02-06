-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for Ord instance comparisons on complex Hydra types

module Hydra.Test.Ordering where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for Ord instance comparisons on complex Hydra types
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "ordering",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Name comparison",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "name less than (alphabetic)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "apple"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "banana"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "name equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hello"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hello"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "name greater than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "zebra"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "apple"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "greaterThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "qualified name less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Type"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "qualified name equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hydra.core.Term"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "name equality true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "name equality false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                  Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Literal comparison",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 literal less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32 literal equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string literal less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "aaa"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bbb"))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean false < true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "boolean true == true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Record comparison (monomorphic)",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person less than by firstName",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person less than by lastName",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Jones"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person less than by age",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latLon less than by lat",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeLatLonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 15.0)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latLon less than by lon",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeLatLonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 25.0)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latLon equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeLatLonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 10.0)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 20.0)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person equality true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person equality false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic type comparison",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly Int32 less than by lat",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly Int32 less than by lon",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly Int32 equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly String less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "10N"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "15N"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly String equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "10N"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "10N"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "20W"))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "PersonOrSomething person vs person",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "age"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                  Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "person"),
                    Core.fieldTerm = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypePersonName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "firstName"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "lastName"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "age"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "PersonOrSomething person equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestTypes.testTypePersonName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "firstName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lastName"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "age"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                  Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "person"),
                    Core.fieldTerm = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypePersonName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "firstName"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "lastName"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "age"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})),
                Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly Int32 equality true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "LatLonPoly Int32 equality false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
              Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Union comparison",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number int variant less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number int variant equal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number float variant less than",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.5)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number float vs int (variant name comparison)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.compare"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 100.0)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "hydra.util.Comparison"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number int equality true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number int equality false (different value)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "Number equality false (different variant)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.equal"))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeNumberName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0)))}}))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
