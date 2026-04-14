-- Note: this is an automatically generated file. Do not edit.

-- | Advanced type checking test cases: annotated terms and flows

module Hydra.Test.Checking.Advanced where

import qualified Hydra.Core as Core
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

-- | Advanced type checking test cases
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Advanced",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        annotatedTermsTests],
      Testing.testGroupCases = []}

annotatedTermsTests :: Testing.TestGroup
annotatedTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Annotated terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        topLevelAnnotationsTests,
        nestedAnnotationsTests,
        annotationsInComplexContextsTests],
      Testing.testGroupCases = []}

annotationsInComplexContextsTests :: Testing.TestGroup
annotationsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Annotations in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
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
                Core.annotatedTermAnnotation = M.empty}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated record fields",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
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
                    Core.annotatedTermAnnotation = M.empty}))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypePersonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated function in application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "add"),
                  Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
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
                  Core.annotatedTermAnnotation = M.empty}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

nestedAnnotationsTests :: Testing.TestGroup
nestedAnnotationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested annotations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotation within annotation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.annotatedTermAnnotation = M.empty})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated terms in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
              Core.annotatedTermAnnotation = M.empty}), (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "hello")),
              Core.annotatedTermAnnotation = M.empty})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated term in function application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.applicationArgument = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                Core.annotatedTermAnnotation = M.empty}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

topLevelAnnotationsTests :: Testing.TestGroup
topLevelAnnotationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Top-level annotations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
              Core.annotatedTermAnnotation = M.empty})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))]),
              Core.annotatedTermAnnotation = M.empty})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
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
              Core.annotatedTermAnnotation = M.empty})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypePersonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.annotatedTermAnnotation = M.empty})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
