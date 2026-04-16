-- Note: this is an automatically generated file. Do not edit.

-- | Fundamental type checking test cases: literals, variables, lambdas, applications, let terms, and primitives

module Hydra.Test.Checking.Fundamentals where

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
import qualified Data.Scientific as Sci
import qualified Data.Map as M

allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Fundamentals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        literalsTests,
        variablesTests,
        lambdasTests,
        applicationsTests,
        letTermsTests,
        primitivesTests],
      Testing.testGroupCases = []}

applicationsInComplexContextsTests :: Testing.TestGroup
applicationsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Applications in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}), (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "b"))})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application in record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypePersonName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "firstName"),
                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "John"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ny"))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "lastName"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
                Core.Field {
                  Core.fieldName = (Core.Name "age"),
                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypePersonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "result"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "result"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested applications",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))}))})),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

applicationsTests :: Testing.TestGroup
applicationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Applications",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleFunctionApplicationsTests,
        partialApplicationsTests,
        higherOrderApplicationsTests,
        polymorphicApplicationsTests,
        applicationsInComplexContextsTests,
        applicationsWithComplexArgumentsTests],
      Testing.testGroupCases = []}

applicationsWithComplexArgumentsTests :: Testing.TestGroup
applicationsWithComplexArgumentsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Applications with complex arguments",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application with record argument",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "getName"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "person"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermProject (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "getName")),
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
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "application with list argument",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "maybeHead"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "xs"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "xs"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "maybeHead")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "first"),
                  (Core.TermLiteral (Core.LiteralString "second"))])}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

binaryPrimitivesTests :: Testing.TestGroup
binaryPrimitivesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Binary primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "math add",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.math.add")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lists cons",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.lists.cons")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                  Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "maps insert",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.maps.insert")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

booleanLiteralsTests :: Testing.TestGroup
booleanLiteralsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Boolean literals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "true",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralBoolean True)))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeBoolean))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "false",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralBoolean False)))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeBoolean))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

floatLiteralsTests :: Testing.TestGroup
floatLiteralsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Float literals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigfloat",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueBigfloat 3.14159))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeBigfloat)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.71828))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "float64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 1.41421))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

higherOrderApplicationsTests :: Testing.TestGroup
higherOrderApplicationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Higher-order applications",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "apply function to function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "apply"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "double"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "apply")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "double"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "function composition",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "compose"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "g"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "add1"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "mul2"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "compose")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "add1"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "mul2"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

higherOrderLambdasTests :: Testing.TestGroup
higherOrderLambdasTests =
    Testing.TestGroup {
      Testing.testGroupName = "Higher-order lambdas",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "function composition",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "f"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "g"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "function application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "f"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "curried function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

higherOrderPrimitivesTests :: Testing.TestGroup
higherOrderPrimitivesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Higher-order primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lists map function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
              Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lists filter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.lists.filter")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                  Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "optionals maybe",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t1"))),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

integerLiteralsTests :: Testing.TestGroup
integerLiteralsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Integer literals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bigint",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int8",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt8 127))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int16",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 32767))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2147483647))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 9223372036854775807))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint8",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint8 255))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint16",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint16 65535))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint32 4294967295))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uint64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 18446744073709551615))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint64)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

lambdasInComplexContextsTests :: Testing.TestGroup
lambdasInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Lambdas in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}),
              (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda in record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "name"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "name"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
              Core.functionTypeCodomain = (Core.TypeVariable TestTypes.testTypePersonName)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

lambdasTests :: Testing.TestGroup
lambdasTests =
    Testing.TestGroup {
      Testing.testGroupName = "Lambdas",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleLambdasTests,
        multiParameterLambdasTests,
        lambdasWithOperationsTests,
        nestedLambdasTests,
        lambdasInComplexContextsTests,
        higherOrderLambdasTests],
      Testing.testGroupCases = []}

lambdasWithOperationsTests :: Testing.TestGroup
lambdasWithOperationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Lambdas with operations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with primitive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "f"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with construction",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

letTermsTests :: Testing.TestGroup
letTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Let terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleLetBindingsTests,
        letTermsWithShadowingTests,
        recursiveBindingsTests,
        mutualRecursionTests,
        nestedLetTermsTests,
        letWithComplexExpressionsTests],
      Testing.testGroupCases = []}

letTermsWithShadowingTests :: Testing.TestGroup
letTermsWithShadowingTests =
    Testing.TestGroup {
      Testing.testGroupName = "Let terms with shadowing",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda parameter shadowing let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested lambda shadowing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermVariable (Core.Name "x")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t1")),
                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple levels of let shadowing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "second")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralBoolean True)),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeBoolean))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let shadowing with lambda and reference to outer binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "y")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "z"))))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                  Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

letWithComplexExpressionsTests :: Testing.TestGroup
letWithComplexExpressionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Let with complex expressions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let in record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypePersonName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "firstName"),
                  Core.fieldTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "first"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "John")),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "middle"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "Q")),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "first"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "middle"))}))}))},
                Core.Field {
                  Core.fieldName = (Core.Name "lastName"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
                Core.Field {
                  Core.fieldName = (Core.Name "age"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypePersonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let in function application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "composition",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "compose"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "g"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "add1"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "double"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "compose")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "add1"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "double"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

literalsInComplexContextsTests :: Testing.TestGroup
literalsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Literals in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "literals in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermPair (Core.TermLiteral (Core.LiteralString "test"), (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14))))))))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeBoolean),
              Core.pairTypeSecond = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                Core.pairTypeSecond = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "literals in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermLiteral (Core.LiteralString "one"),
              (Core.TermLiteral (Core.LiteralString "two")),
              (Core.TermLiteral (Core.LiteralString "three"))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

literalsTests :: Testing.TestGroup
literalsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Literals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        booleanLiteralsTests,
        stringLiteralsTests,
        integerLiteralsTests,
        floatLiteralsTests,
        literalsInComplexContextsTests],
      Testing.testGroupCases = []}

monomorphicVsPolymorphicTests :: Testing.TestGroup
monomorphicVsPolymorphicTests =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic vs polymorphic",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "monomorphic math",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.math.add")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic identity",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.equality.identity")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.lists.map")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterLambdasTests :: Testing.TestGroup
multiParameterLambdasTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter lambdas",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two parameters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "three parameters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "parameter reuse",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                        Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

mutualRecursionTests :: Testing.TestGroup
mutualRecursionTests =
    Testing.TestGroup {
      Testing.testGroupName = "Mutual recursion",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mutually recursive data",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "listA"),
                  Core.bindingTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeBuddyListAName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "head"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "tail"),
                        Core.fieldTerm = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "listB"))))}]})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "listB"),
                  Core.bindingTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeBuddyListBName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "head"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "tail"),
                        Core.fieldTerm = (Core.TermMaybe Nothing)}]})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "listA"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "(monomorphic) mutually recursive functions",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

nestedLambdasTests :: Testing.TestGroup
nestedLambdasTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested lambdas",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda returning lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermVariable (Core.Name "x")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "y"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with inner lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "outer"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "inner"),
                    Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "inner")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

nestedLetTermsTests :: Testing.TestGroup
nestedLetTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested let terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "monomorphic nesting",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic nesting",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "apply"),
                    Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "f"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "apply")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "id"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "test"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable capture avoidance",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "x")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "y"))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple let in lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "z"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermVariable (Core.Name "z")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "y"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

nullaryPrimitivesTests :: Testing.TestGroup
nullaryPrimitivesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nullary primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.maps.empty")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.sets.empty")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

partialApplicationsTests :: Testing.TestGroup
partialApplicationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Partial applications",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "partially applied add",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "partially applied string cat",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "prefix"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicApplicationsTests :: Testing.TestGroup
polymorphicApplicationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic applications",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic identity",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic const",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "const"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "const")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "keep"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 999)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic flip",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "flip"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "flip")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicVariablesTests :: Testing.TestGroup
polymorphicVariablesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic variables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "id"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "test"))}))))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "higher order polymorphic",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "apply"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "apply"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

primitivesInComplexContextsTests :: Testing.TestGroup
primitivesInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Primitives in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "primitive composition",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "double"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.mul")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "increment"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "double"))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "increment"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested higher-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

primitivesTests :: Testing.TestGroup
primitivesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        nullaryPrimitivesTests,
        unaryPrimitivesTests,
        binaryPrimitivesTests,
        ternaryPrimitivesTests,
        monomorphicVsPolymorphicTests,
        higherOrderPrimitivesTests,
        primitivesInComplexContextsTests],
      Testing.testGroupCases = []}

recursiveBindingsTests :: Testing.TestGroup
recursiveBindingsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Recursive bindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple arithmetic recursion",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "double"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "n"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "double")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recursiveVariablesTests :: Testing.TestGroup
recursiveVariablesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Recursive variables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple recursion",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mutual recursion",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleFunctionApplicationsTests :: Testing.TestGroup
simpleFunctionApplicationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple function applications",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "primitive application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string concatenation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleLambdasTests :: Testing.TestGroup
simpleLambdasTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple lambdas",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "constant function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleLetBindingsTests :: Testing.TestGroup
simpleLetBindingsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple let bindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleVariableLookupTests :: Testing.TestGroup
simpleVariableLookupTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple variable lookup",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
              Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

stringLiteralsTests :: Testing.TestGroup
stringLiteralsTests =
    Testing.TestGroup {
      Testing.testGroupName = "String literals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralString "hello")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralString "")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralString "caf\233")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

ternaryPrimitivesTests :: Testing.TestGroup
ternaryPrimitivesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Ternary primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "logic ifElse",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lists foldl",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t1"))),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unaryPrimitivesTests :: Testing.TestGroup
unaryPrimitivesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Unary primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lists maybeHead",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "math neg",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.math.negate")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "logic not",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermVariable (Core.Name "hydra.lib.logic.not")))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

variableScopingTests :: Testing.TestGroup
variableScopingTests =
    Testing.TestGroup {
      Testing.testGroupName = "Variable scoping",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let binding scope",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable shadowing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested scoping",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermVariable (Core.Name "x")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermPair (Core.TermVariable (Core.Name "y"), (Core.TermVariable (Core.Name "z"))))))}))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                      Core.pairTypeSecond = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                        Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

variablesInComplexContextsTests :: Testing.TestGroup
variablesInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Variables in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable in record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "name"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypePersonName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "name"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
              Core.functionTypeCodomain = (Core.TypeVariable TestTypes.testTypePersonName)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                (Core.TermVariable (Core.Name "x"))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable in map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "key"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "value"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermMap (M.fromList [
                  (Core.TermVariable (Core.Name "key"), (Core.TermVariable (Core.Name "value")))]))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                      Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                      Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "variable in optional",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

variablesTests :: Testing.TestGroup
variablesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Variables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleVariableLookupTests,
        variableScopingTests,
        polymorphicVariablesTests,
        variablesInComplexContextsTests,
        recursiveVariablesTests],
      Testing.testGroupCases = []}
