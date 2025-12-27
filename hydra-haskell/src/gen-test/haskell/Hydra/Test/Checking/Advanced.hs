-- Note: this is an automatically generated file. Do not edit.

-- | Advanced type checking test cases: annotated terms and flows

module Hydra.Test.Checking.Advanced where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Advanced type checking test cases
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Advanced",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    annotatedTermsTests,
    flowsTests],
  Testing.testGroupCases = []}

annotatedTermsTests :: Testing.TestGroup
annotatedTermsTests = Testing.TestGroup {
  Testing.testGroupName = "Annotated terms",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    topLevelAnnotationsTests,
    nestedAnnotationsTests,
    annotationsInComplexContextsTests],
  Testing.testGroupCases = []}

topLevelAnnotationsTests :: Testing.TestGroup
topLevelAnnotationsTests = Testing.TestGroup {
  Testing.testGroupName = "Top-level annotations",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated literal",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermList [
            Core.TermLiteral (Core.LiteralString "a"),
            (Core.TermLiteral (Core.LiteralString "b"))]),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermList [
            Core.TermLiteral (Core.LiteralString "a"),
            (Core.TermLiteral (Core.LiteralString "b"))]),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated record",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermRecord (Core.Record {
            Core.recordTypeName = TestTypes.testTypePersonName,
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "firstName"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "John"))},
              Core.Field {
                Core.fieldName = (Core.Name "lastName"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
              Core.Field {
                Core.fieldName = (Core.Name "age"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermRecord (Core.Record {
            Core.recordTypeName = TestTypes.testTypePersonName,
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "firstName"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "John"))},
              Core.Field {
                Core.fieldName = (Core.Name "lastName"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
              Core.Field {
                Core.fieldName = (Core.Name "age"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeVariable TestTypes.testTypePersonName)})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t0"),
            Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

nestedAnnotationsTests :: Testing.TestGroup
nestedAnnotationsTests = Testing.TestGroup {
  Testing.testGroupName = "Nested annotations",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotation within annotation",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))),
            Core.annotatedTermAnnotation = M.empty})),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))),
            Core.annotatedTermAnnotation = M.empty})),
          Core.annotatedTermAnnotation = M.empty})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated terms in tuple",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
          Core.annotatedTermAnnotation = M.empty}), (Core.TermAnnotated (Core.AnnotatedTerm {
          Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "hello")),
          Core.annotatedTermAnnotation = M.empty})))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
              Core.annotatedTermAnnotation = M.empty}), (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "hello")),
              Core.annotatedTermAnnotation = M.empty})))),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated term in function application",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
            Core.annotatedTermAnnotation = M.empty})),
          Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Core.annotatedTermAnnotation = M.empty}))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
            Core.annotatedTermAnnotation = M.empty})),
          Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Core.annotatedTermAnnotation = M.empty}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

annotationsInComplexContextsTests :: Testing.TestGroup
annotationsInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Annotations in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated let binding",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "x"),
              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.bindingType = Nothing},
            Core.Binding {
              Core.bindingName = (Core.Name "y"),
              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "world")),
                Core.annotatedTermAnnotation = M.empty})),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y")))),
            Core.annotatedTermAnnotation = M.empty}))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "x"),
              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.typeSchemeConstraints = Nothing}))},
            Core.Binding {
              Core.bindingName = (Core.Name "y"),
              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "world")),
                Core.annotatedTermAnnotation = M.empty})),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                Core.typeSchemeConstraints = Nothing}))}],
          Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
            Core.annotatedTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y")))),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.annotatedTermAnnotation = M.empty}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated record fields",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermRecord (Core.Record {
          Core.recordTypeName = TestTypes.testTypePersonName,
          Core.recordFields = [
            Core.Field {
              Core.fieldName = (Core.Name "firstName"),
              Core.fieldTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "Alice")),
                Core.annotatedTermAnnotation = M.empty}))},
            Core.Field {
              Core.fieldName = (Core.Name "lastName"),
              Core.fieldTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "Smith")),
                Core.annotatedTermAnnotation = M.empty}))},
            Core.Field {
              Core.fieldName = (Core.Name "age"),
              Core.fieldTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))),
                Core.annotatedTermAnnotation = M.empty}))}]})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermRecord (Core.Record {
          Core.recordTypeName = TestTypes.testTypePersonName,
          Core.recordFields = [
            Core.Field {
              Core.fieldName = (Core.Name "firstName"),
              Core.fieldTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "Alice")),
                Core.annotatedTermAnnotation = M.empty}))},
            Core.Field {
              Core.fieldName = (Core.Name "lastName"),
              Core.fieldTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "Smith")),
                Core.annotatedTermAnnotation = M.empty}))},
            Core.Field {
              Core.fieldName = (Core.Name "age"),
              Core.fieldTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))),
                Core.annotatedTermAnnotation = M.empty}))}]})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeVariable TestTypes.testTypePersonName)})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "annotated function in application",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "add"),
              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "add")),
              Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))),
                Core.annotatedTermAnnotation = M.empty}))})),
            Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
              Core.annotatedTermAnnotation = M.empty}))}))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "add"),
              Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                Core.typeSchemeConstraints = Nothing}))}],
          Core.letBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "add")),
              Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))),
                Core.annotatedTermAnnotation = M.empty}))})),
            Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
              Core.annotatedTermAnnotation = M.empty}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

flowsTests :: Testing.TestGroup
flowsTests = Testing.TestGroup {
  Testing.testGroupName = "Flows",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    flowsWithFailureAcrossLetBindingsTests],
  Testing.testGroupCases = []}

flowsWithFailureAcrossLetBindingsTests :: Testing.TestGroup
flowsWithFailureAcrossLetBindingsTests = Testing.TestGroup {
  Testing.testGroupName = "Flows with failure across let bindings",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "mutually referential failure functions with Flow monad",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "conditionalUnexpected"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "s"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "b"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "ignored"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "unexpected")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "oops"))}))})),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "unexpected")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))})))})))}))),
              Core.bindingType = Nothing},
            Core.Binding {
              Core.bindingName = (Core.Name "unexpected"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "s"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.fail"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "conditionalUnexpected"))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "t2"),
              Core.typeLambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "conditionalUnexpected"),
                    Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t3"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "t4"),
                        Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                          Core.typeLambdaParameter = (Core.Name "t5"),
                          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "s"),
                            Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "b"),
                              Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeBoolean)),
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "ignored"),
                                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t3"))),
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                                        Core.typeApplicationTermType = (Core.TypeApplication (Core.ApplicationType {
                                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                            Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.Flow")),
                                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t4"))})),
                                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t5"))}))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                          Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "unexpected")),
                                          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t4"))})),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t5"))})),
                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "oops"))}))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                      Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                        Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "unexpected")),
                                        Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t4"))})),
                                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t5"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))})))})))})))}))}))})),
                    Core.bindingType = (Just (Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t3",
                        Core.Name "t4",
                        (Core.Name "t5")],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t3")),
                            Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.Flow")),
                                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t4"))})),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t5"))}))}))}))})),
                      Core.typeSchemeConstraints = Nothing}))},
                  Core.Binding {
                    Core.bindingName = (Core.Name "unexpected"),
                    Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t3"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "t4"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                                Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.flows.fail"))),
                                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t3"))})),
                              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t4"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))}))})),
                    Core.bindingType = (Just (Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t3",
                        (Core.Name "t4")],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.Flow")),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t3"))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t4"))}))})),
                      Core.typeSchemeConstraints = Nothing}))}],
                Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "conditionalUnexpected")),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t2"))}))}))}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t2"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.compute.Flow")),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))}))}))}))}))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}
