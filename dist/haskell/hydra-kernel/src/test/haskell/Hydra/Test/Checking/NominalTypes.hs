-- Note: this is an automatically generated file. Do not edit.

-- | Nominal type checking test cases: records, unions, field access, injection, projection

module Hydra.Test.Checking.NominalTypes where

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

allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nominal types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        recordsTests,
        unionsTests,
        wrappedTermsTests,
        eliminationsTests],
      Testing.testGroupCases = []}

chainedUnwrappingTests :: Testing.TestGroup
chainedUnwrappingTests =
    Testing.TestGroup {
      Testing.testGroupName = "Chained unwrapping",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap then process",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "wrapped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "wrapped"))}))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " suffix"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeStringAliasName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap polymorphic then map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "wrappedList"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypePolymorphicWrapperName))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "wrappedList"))}))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

eliminationsTests :: Testing.TestGroup
eliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        recordEliminationsTests,
        unionEliminationsTests,
        wrapEliminationsTests],
      Testing.testGroupCases = []}

higherOrderRecordProjectionsTests :: Testing.TestGroup
higherOrderRecordProjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Higher-order record projections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map projection over list of records",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "firstName")}))))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermRecord (Core.Record {
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
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}),
                (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Jones"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map polymorphic projection",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.projectionField = (Core.Name "lat")}))))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 40)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-74))))}]}),
                (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lat"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 34)))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lon"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-118))))}]}))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "filter using projection",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "person"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = TestTypes.testTypePersonName,
                          Core.projectionField = (Core.Name "age")})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}))})))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermRecord (Core.Record {
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
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 35)))}]}),
                (Core.TermRecord (Core.Record {
                  Core.recordTypeName = TestTypes.testTypePersonName,
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Jones"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeVariable TestTypes.testTypePersonName)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

higherOrderUnionEliminationsTests :: Testing.TestGroup
higherOrderUnionEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Higher-order union eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map match over list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "lessThan"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "less"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "equalTo"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "equal"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "greaterThan"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "greater"))})))}]}))))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeComparisonName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "lessThan"),
                    Core.fieldTerm = Core.TermUnit}}),
                (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypeComparisonName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "equalTo"),
                    Core.fieldTerm = Core.TermUnit}}))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "compose match with other functions",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "comp"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.length")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lessThan"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "less"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "equalTo"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "equal"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "greaterThan"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "greater"))})))}]})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "comp"))}))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeComparisonName),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match in lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "unionValue"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestTypes.testTypeNumberName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "int"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "i"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "i"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "float"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "f"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "unionValue"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeNumberName),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

monomorphicRecordsTests :: Testing.TestGroup
monomorphicRecordsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic records",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latlon record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "LatLon"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 19.5429)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-155.6659))))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable (Core.Name "LatLon")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latlon with variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "LatLon"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 19.5429)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}]}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "LatLon"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Person"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "firstName"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                Core.Field {
                  Core.fieldName = (Core.Name "lastName"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                Core.Field {
                  Core.fieldName = (Core.Name "age"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable (Core.Name "Person")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Unit"),
              Core.recordFields = []})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable (Core.Name "Unit")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "person with variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "name"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "age"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Person"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "firstName"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "name"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "lastName"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Doe"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "age"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "age"))}]}))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "Person"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

monomorphicUnwrappingTests :: Testing.TestGroup
monomorphicUnwrappingTests =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic unwrapping",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap string alias",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeStringAliasName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

monomorphicWrappedTermsTests :: Testing.TestGroup
monomorphicWrappedTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic wrapped terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string alias",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hello"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeStringAliasName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped integer",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrapped"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeStringAliasName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "first"))}), (Core.TermLiteral (Core.LiteralString "second")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable TestTypes.testTypeStringAliasName),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterPolymorphicCaseStatementsTests :: Testing.TestGroup
multiParameterPolymorphicCaseStatementsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter polymorphic case statements",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case Either converting both to string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeEitherName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "left"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "right"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showFloat32")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))})),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case Either applied to injection",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = TestTypes.testTypeEitherName,
                Core.caseStatementDefault = Nothing,
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "left"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "n"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "right"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "s"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.length")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})))}]})))),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeEitherName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "left"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case Either with Triple and nested projections",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "triple"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestTypes.testTypeEitherName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "left"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "coords"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
                            Core.projectionField = (Core.Name "lat")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "coords"))}))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "right"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "t"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypeTripleName,
                            Core.projectionField = (Core.Name "first")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})))}]})))),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = TestTypes.testTypeTripleName,
                    Core.projectionField = (Core.Name "second")})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "triple"))}))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "t3"),
                    Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                      Core.forallTypeParameter = (Core.Name "t4"),
                      Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeTripleName),
                              Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                            Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                                Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeLatLonPolyName),
                                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))})),
                              Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                    Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeTripleName),
                                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))})),
                                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t3"))}))}))})),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t4"))})),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case Either with polymorphic let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "makeLeft"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestTypes.testTypeEitherName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "left"),
                        Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "makeRight"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestTypes.testTypeEitherName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "right"),
                        Core.fieldTerm = (Core.TermVariable (Core.Name "y"))}}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "flag"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeEitherName,
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "left"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "n"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "makeRight")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "right"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "makeLeft")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.length")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))})))}]})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "flag"))}))})))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterPolymorphicInjectionsTests :: Testing.TestGroup
multiParameterPolymorphicInjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter polymorphic injections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "either left with int",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeEitherName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "left"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "either right with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeEitherName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "right"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "hello"))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "either containing LatLonPoly in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeEitherName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "right"),
                Core.fieldTerm = (Core.TermList [
                  Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeLatLonPolyName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lat"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 40)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lon"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-74))))}]})])}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.applicationTypeArgument = (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeLatLonPolyName),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "either in triple in map with shared type variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x0"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x1"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x2"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermMap (M.fromList [
                    (Core.TermLiteral (Core.LiteralString "key"), (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypeTripleName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "first"),
                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = TestTypes.testTypeEitherName,
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "left"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x0"))}}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "second"),
                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = TestTypes.testTypeEitherName,
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "left"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x0"))}}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "third"),
                          Core.fieldTerm = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = TestTypes.testTypeEitherName,
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "right"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x1"))}}))}]})))]))})))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "t3"),
                    Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                      Core.forallTypeParameter = (Core.Name "t4"),
                      Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                        Core.forallTypeParameter = (Core.Name "t5"),
                        Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                              Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                                Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.mapTypeValues = (Core.TypeApplication (Core.ApplicationType {
                                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                      Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeTripleName),
                                      Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                          Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t3"))}))})),
                                    Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                        Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t4"))}))})),
                                  Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                                      Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeEitherName),
                                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t5"))})),
                                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))}))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterPolymorphicProjectionsTests :: Testing.TestGroup
multiParameterPolymorphicProjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter polymorphic projections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project first from Triple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeTripleName,
              Core.projectionField = (Core.Name "first")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeTripleName),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t2"))})),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project second from Triple applied",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypeTripleName,
                Core.projectionField = (Core.Name "second")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeTripleName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "first"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "second"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "middle"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "third"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean True))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project from Triple and use second field, which is another polymorphic record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "triple"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "key"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "person"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "p"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermMaybe Nothing)})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "m"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.lookup")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "key"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "m"))}))})))}]})))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = TestTypes.testTypeTripleName,
                      Core.projectionField = (Core.Name "second")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "triple"))}))}))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "t2"),
                  Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "t3"),
                    Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeTripleName),
                            Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
                            Core.applicationTypeArgument = (Core.TypeMap (Core.MapType {
                              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t1")),
                              Core.mapTypeValues = (Core.TypeVariable (Core.Name "t2"))}))}))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t3"))})),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                        Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t2")))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterPolymorphicRecordsTests :: Testing.TestGroup
multiParameterPolymorphicRecordsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter polymorphic records",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "triple with three monomorphic types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Triple"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "first"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "second"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "middle"))},
                Core.Field {
                  Core.fieldName = (Core.Name "third"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean True))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "Triple")),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeBoolean)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "triple with PersonOrSomething containing map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "k"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "v"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Triple"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "first"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "prefix"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "second"),
                      Core.fieldTerm = (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "other"),
                          Core.fieldTerm = (Core.TermMap (M.fromList [
                            (Core.TermVariable (Core.Name "k"), (Core.TermVariable (Core.Name "v")))]))}}))},
                    Core.Field {
                      Core.fieldName = (Core.Name "third"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 999)))}]}))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "Triple")),
                          Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
                        Core.applicationTypeArgument = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "PersonOrSomething")),
                          Core.applicationTypeArgument = (Core.TypeMap (Core.MapType {
                            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                            Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                      Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterPolymorphicUnwrappersTests :: Testing.TestGroup
multiParameterPolymorphicUnwrappersTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter polymorphic unwrappers",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap symmetric triple to tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "st"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypeTripleName,
                  Core.projectionField = (Core.Name "first")})))),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeSymmetricTripleName))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "st"))}))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypeTripleName,
                  Core.projectionField = (Core.Name "third")})))),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeSymmetricTripleName))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "st"))}))}))))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeSymmetricTripleName),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))})),
                  Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                    Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t0"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap and collect edges in set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "getEdge"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "st"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypeTripleName,
                        Core.projectionField = (Core.Name "second")})))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeSymmetricTripleName))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "st"))}))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "triples"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.sets.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "getEdge"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "triples"))}))})))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeSymmetricTripleName),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))),
                  Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t1")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap with maybe to handle optional symmetric triple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "mst"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                    Core.applicationArgument = (Core.TermMaybe Nothing)})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "st"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermMaybe (Just (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypeTripleName,
                        Core.projectionField = (Core.Name "second")})))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeSymmetricTripleName))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "st"))}))}))))})))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "mst"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeMaybe (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeSymmetricTripleName),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))),
                  Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t1")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multiParameterPolymorphicWrappersTests :: Testing.TestGroup
multiParameterPolymorphicWrappersTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multi-parameter polymorphic wrappers",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "symmetric triple wrapping simple types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypeSymmetricTripleName,
              Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeTripleName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "first"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "second"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "edge"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "third"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeSymmetricTripleName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "symmetric triple from lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v1"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "e"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v2"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = TestTypes.testTypeSymmetricTripleName,
                    Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypeTripleName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "first"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "v1"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "second"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "e"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "third"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "v2"))}]}))}))})))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeSymmetricTripleName),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "symmetric triple with nested polymorphic types and foldl",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "sumList"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "lst"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                          Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "acc"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "acc"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "lst"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "nums1"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "nums2"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = TestTypes.testTypeSymmetricTripleName,
                    Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestTypes.testTypeTripleName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "first"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "sumList")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "nums1"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "second"),
                          Core.fieldTerm = (Core.TermList [
                            Core.TermVariable (Core.Name "nums1"),
                            (Core.TermVariable (Core.Name "nums2"))])},
                        Core.Field {
                          Core.fieldName = (Core.Name "third"),
                          Core.fieldTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "sumList")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "nums2"))}))}]}))}))})))})))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeSymmetricTripleName),
                    Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.applicationTypeArgument = (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multipleUnwrapOperationsTests :: Testing.TestGroup
multipleUnwrapOperationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multiple unwrap operations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap different types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "stringWrapped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "listWrapped"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermPair (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "stringWrapped"))}), (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypePolymorphicWrapperName))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "listWrapped"))}))))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeStringAliasName),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                    Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

multipleWrappingLevelsTests :: Testing.TestGroup
multipleWrappingLevelsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Multiple wrapping levels",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped in optional",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMaybe (Just (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "wrapped"))})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMaybe (Core.TypeVariable TestTypes.testTypeStringAliasName)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of wrapped polymorphic",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
                Core.wrappedTermBody = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])}),
              (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
                Core.wrappedTermBody = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))])}))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

nestedUnionEliminationsTests :: Testing.TestGroup
nestedUnionEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested union eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested match statements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypePersonOrSomethingName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "person"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "p"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "other"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = TestTypes.testTypeNumberName,
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "int"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "i"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "float"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "f"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showFloat32")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
                Core.applicationTypeArgument = (Core.TypeVariable TestTypes.testTypeNumberName)})),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "lessThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "equalTo"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "greaterThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})))}]}))), (Core.TermLiteral (Core.LiteralString "context")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeComparisonName),
                Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

nestedWrappedTermsTests :: Testing.TestGroup
nestedWrappedTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested wrapped terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
              Core.wrappedTermBody = (Core.TermList [
                Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "a")))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
              Core.applicationTypeArgument = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped optional",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
              Core.wrappedTermBody = (Core.TermList [
                Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
              Core.applicationTypeArgument = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
              Core.wrappedTermBody = (Core.TermList [
                Core.TermMap (M.fromList [
                  (Core.TermLiteral (Core.LiteralString "key"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
              Core.applicationTypeArgument = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicRecordProjectionsAppliedTests :: Testing.TestGroup
polymorphicRecordProjectionsAppliedTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic record projections applied",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lat from LatLonPoly with int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
                Core.projectionField = (Core.Name "lat")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "LatLonPoly"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 40)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-74))))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lon from LatLonPoly with float64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
                Core.projectionField = (Core.Name "lon")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "LatLonPoly"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 40.7128)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-74.006))))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project head from BuddyListA with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypeBuddyListAName,
                Core.projectionField = (Core.Name "head")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "BuddyListA"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "head"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "tail"),
                    Core.fieldTerm = (Core.TermMaybe Nothing)}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicRecordProjectionsTests :: Testing.TestGroup
polymorphicRecordProjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic record projections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lat from polymorphic LatLonPoly",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
              Core.projectionField = (Core.Name "lat")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lon from polymorphic LatLonPoly",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
              Core.projectionField = (Core.Name "lon")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project head from BuddyListA",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeBuddyListAName,
              Core.projectionField = (Core.Name "head")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "BuddyListA")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project tail from BuddyListA",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeBuddyListAName,
              Core.projectionField = (Core.Name "tail")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "BuddyListA")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "BuddyListB")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicRecordsTests :: Testing.TestGroup
polymorphicRecordsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic records",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latlon poly float",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "LatLonPoly"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 19.5429)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-155.6659))))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latlon poly int64",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "LatLonPoly"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 195429)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt64 (-1556659))))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "latlon poly variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "LatLonPoly"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}]}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "buddylist string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "BuddyListA"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "head"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "first"))},
                Core.Field {
                  Core.fieldName = (Core.Name "tail"),
                  Core.fieldTerm = (Core.TermMaybe Nothing)}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "BuddyListA")),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "buddylist variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "BuddyListA"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "head"),
                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "tail"),
                    Core.fieldTerm = (Core.TermMaybe Nothing)}]}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "BuddyListA")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicRecursiveUnionInjectionsTests :: Testing.TestGroup
polymorphicRecursiveUnionInjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic recursive union injections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject boolean into UnionPolymorphicRecursive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "bool"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean True))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject string value into UnionPolymorphicRecursive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "value"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "test"))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject int value into UnionPolymorphicRecursive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "value"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 123)))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicUnionEliminationsTests :: Testing.TestGroup
polymorphicUnionEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic union eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simplePolymorphicUnionTests,
        usingUnionPolymorphicRecursiveTests,
        usingKernelTypesTests],
      Testing.testGroupCases = []}

polymorphicUnionInjectionsTests :: Testing.TestGroup
polymorphicUnionInjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic union injections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject person into PersonOrSomething",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
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
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
                Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject string into PersonOrSomething other variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "other"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "something else"))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject int into PersonOrSomething other variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "other"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicUnionsFromLambdaTests :: Testing.TestGroup
polymorphicUnionsFromLambdaTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic unions from lambda",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda creating PersonOrSomething other variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "other"),
                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda creating UnionPolymorphicRecursive value variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "value"),
                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))}}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicUnwrappingTests :: Testing.TestGroup
polymorphicUnwrappingTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic unwrapping",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap polymorphic wrapper",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypePolymorphicWrapperName))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

polymorphicWrappedTermsTests :: Testing.TestGroup
polymorphicWrappedTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic wrapped terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic wrapper with int",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
              Core.wrappedTermBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
              Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic wrapper with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
              Core.wrappedTermBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic wrapper from lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
                Core.wrappedTermBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x")])}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePolymorphicWrapperName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

projectionsWithVariablesTests :: Testing.TestGroup
projectionsWithVariablesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Projections with variables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project from lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "person"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "firstName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project from polymorphic lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "coords"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.projectionField = (Core.Name "lat")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "coords"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple projections from same record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "person"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "firstName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "lastName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recordEliminationsTests :: Testing.TestGroup
recordEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Record eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleRecordProjectionsTests,
        recordProjectionsAppliedToRecordsTests,
        polymorphicRecordProjectionsTests,
        polymorphicRecordProjectionsAppliedTests,
        recordProjectionsWithVariablesTests,
        recordProjectionsInComplexContextsTests,
        multiParameterPolymorphicProjectionsTests,
        higherOrderRecordProjectionsTests,
        recursiveRecordProjectionsTests,
        recordProjectionsWithMutualRecursionTests,
        projectionsWithVariablesTests],
      Testing.testGroupCases = []}

recordProjectionsAppliedToRecordsTests :: Testing.TestGroup
recordProjectionsAppliedToRecordsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Record projections applied to records",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project firstName applied to person record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypePersonName,
                Core.projectionField = (Core.Name "firstName")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "Person"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Alice"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Smith"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project age applied to person record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypePersonName,
                Core.projectionField = (Core.Name "age")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "Person"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "firstName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lastName"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Jones"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "age"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lat applied to LatLon record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypeLatLonName,
                Core.projectionField = (Core.Name "lat")})))),
              Core.applicationArgument = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "LatLon"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lat"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 40.7128)))},
                  Core.Field {
                    Core.fieldName = (Core.Name "lon"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (-74.006))))}]}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recordProjectionsInComplexContextsTests :: Testing.TestGroup
recordProjectionsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Record projections in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "projection in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "person"),
                  Core.bindingTerm = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "Person"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "firstName"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Charlie"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "lastName"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Brown"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "age"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 35)))}]})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "getName"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = TestTypes.testTypePersonName,
                    Core.projectionField = (Core.Name "firstName")})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "getName")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "projection in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypePersonName,
              Core.projectionField = (Core.Name "firstName")}))), (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypePersonName,
              Core.projectionField = (Core.Name "age")})))))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
                Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "projection in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypePersonName,
                Core.projectionField = (Core.Name "firstName")}))),
              (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                Core.projectionTypeName = TestTypes.testTypePersonName,
                Core.projectionField = (Core.Name "lastName")}))))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recordProjectionsWithMutualRecursionTests :: Testing.TestGroup
recordProjectionsWithMutualRecursionTests =
    Testing.TestGroup {
      Testing.testGroupName = "Record projections with mutual recursion",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project head from BuddyListA",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeBuddyListAName,
              Core.projectionField = (Core.Name "head")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project tail from BuddyListB",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeBuddyListBName,
              Core.projectionField = (Core.Name "tail")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListBName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "chained projections across mutual recursion",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "listA"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                    Core.applicationArgument = (Core.TermMaybe Nothing)})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "listB"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                          Core.applicationArgument = (Core.TermMaybe Nothing)})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = TestTypes.testTypeBuddyListAName,
                          Core.projectionField = (Core.Name "tail")}))))})),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = TestTypes.testTypeBuddyListBName,
                          Core.projectionField = (Core.Name "tail")})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "listB"))}))}))})))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = TestTypes.testTypeBuddyListAName,
                    Core.projectionField = (Core.Name "tail")})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "listA"))}))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListBName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recordProjectionsWithVariablesTests :: Testing.TestGroup
recordProjectionsWithVariablesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Record projections with variables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project from lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "person"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "firstName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project from polymorphic lambda parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "coords"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypeLatLonPolyName,
                  Core.projectionField = (Core.Name "lat")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "coords"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple projections from same record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "person"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "firstName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = TestTypes.testTypePersonName,
                  Core.projectionField = (Core.Name "lastName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "person"))}))))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recordsInComplexContextsTests :: Testing.TestGroup
recordsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Records in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "records in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Person"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "firstName"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Bob"))},
                Core.Field {
                  Core.fieldName = (Core.Name "lastName"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "Jones"))},
                Core.Field {
                  Core.fieldName = (Core.Name "age"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]}), (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "LatLon"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.0)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.0)))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "Person")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "LatLon"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "poly records in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "LatLonPoly"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "lat"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "lon"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]}), (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "BuddyListA"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "head"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "test"))},
                Core.Field {
                  Core.fieldName = (Core.Name "tail"),
                  Core.fieldTerm = (Core.TermMaybe Nothing)}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "LatLonPoly")),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.pairTypeSecond = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "BuddyListA")),
                Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "recursive record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "IntList"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "head"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                Core.Field {
                  Core.fieldName = (Core.Name "tail"),
                  Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                    Core.recordTypeName = (Core.Name "IntList"),
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "head"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))},
                      Core.Field {
                        Core.fieldName = (Core.Name "tail"),
                        Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable (Core.Name "IntList")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recordsTests :: Testing.TestGroup
recordsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Records",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        monomorphicRecordsTests,
        polymorphicRecordsTests,
        recordsInComplexContextsTests,
        multiParameterPolymorphicRecordsTests],
      Testing.testGroupCases = []}

recursiveRecordProjectionsTests :: Testing.TestGroup
recursiveRecordProjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Recursive record projections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested projection from recursive record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "intList"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.maybe")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = TestTypes.testTypeIntListName,
                    Core.projectionField = (Core.Name "head")}))))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                    Core.projectionTypeName = TestTypes.testTypeIntListName,
                    Core.projectionField = (Core.Name "tail")})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "intList"))}))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeIntListName),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

recursiveUnionEliminationsTests :: Testing.TestGroup
recursiveUnionEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Recursive union eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match HydraType recursively",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeHydraTypeName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "literal"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "lit"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = TestTypes.testTypeHydraLiteralTypeName,
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "boolean"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "b"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showBoolean")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "string"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "s"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "lit"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "list"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "nested"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "list"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeHydraTypeName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simplePolymorphicUnionTests :: Testing.TestGroup
simplePolymorphicUnionTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple polymorphic unions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match PersonOrSomething with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypePersonOrSomethingName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "person"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "p"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                        Core.projectionTypeName = TestTypes.testTypePersonName,
                        Core.projectionField = (Core.Name "firstName")})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "other"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
                Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match PersonOrSomething instantiated with string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = TestTypes.testTypePersonOrSomethingName,
                Core.caseStatementDefault = Nothing,
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "person"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "p"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                          Core.projectionTypeName = TestTypes.testTypePersonName,
                          Core.projectionField = (Core.Name "firstName")})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}]})))),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "other"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "test"))}}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleRecordProjectionsTests :: Testing.TestGroup
simpleRecordProjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple record projections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project firstName from Person",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypePersonName,
              Core.projectionField = (Core.Name "firstName")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lastName from Person",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypePersonName,
              Core.projectionField = (Core.Name "lastName")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project age from Person",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypePersonName,
              Core.projectionField = (Core.Name "age")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "Person")),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lat from LatLon",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeLatLonName,
              Core.projectionField = (Core.Name "lat")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "LatLon")),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "project lon from LatLon",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = TestTypes.testTypeLatLonName,
              Core.projectionField = (Core.Name "lon")})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "LatLon")),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleUnionInjectionsTests :: Testing.TestGroup
simpleUnionInjectionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple union injections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Comparison lessThan variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeComparisonName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "lessThan"),
                Core.fieldTerm = Core.TermUnit}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeComparisonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Comparison equalTo variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeComparisonName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "equalTo"),
                Core.fieldTerm = Core.TermUnit}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeComparisonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Comparison greaterThan variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeComparisonName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "greaterThan"),
                Core.fieldTerm = Core.TermUnit}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeComparisonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

simpleUnitVariantEliminationsTests :: Testing.TestGroup
simpleUnitVariantEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Simple unit inject eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Comparison with all cases",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "lessThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "less"))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "equalTo"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "equal"))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "greaterThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "greater"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeComparisonName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Comparison returning int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "lessThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "equalTo"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "greaterThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeComparisonName),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match applied to Comparison variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
                Core.caseStatementDefault = Nothing,
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "lessThan"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "less"))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "equalTo"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "equal"))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "greaterThan"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "greater"))})))}]})))),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeComparisonName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "equalTo"),
                  Core.fieldTerm = Core.TermUnit}}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unionEliminationsInComplexContextsTests :: Testing.TestGroup
unionEliminationsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Union eliminations in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "matcher"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "lessThan"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "less"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "equalTo"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "equal"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "greaterThan"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "greater"))})))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "matcher"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeComparisonName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match in record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
              Core.recordTypeName = TestTypes.testTypePersonName,
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "firstName"),
                  Core.fieldTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = TestTypes.testTypePersonOrSomethingName,
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "person"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "p"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                Core.projectionTypeName = TestTypes.testTypePersonName,
                                Core.projectionField = (Core.Name "firstName")})))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "other"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}]})))),
                    Core.applicationArgument = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "other"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "John"))}}))}))},
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
          Testing.testCaseWithMetadataName = "match with polymorphic result in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestTypes.testTypePersonOrSomethingName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "person"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "p"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                            Core.projectionTypeName = TestTypes.testTypePersonName,
                            Core.projectionField = (Core.Name "age")})))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "p"))}))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "other"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}]})))),
                Core.applicationArgument = (Core.TermUnion (Core.Injection {
                  Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                  Core.injectionField = Core.Field {
                    Core.fieldName = (Core.Name "other"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}}))}),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unionEliminationsTests :: Testing.TestGroup
unionEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Union eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleUnitVariantEliminationsTests,
        unionEliminationsWithDataTests,
        polymorphicUnionEliminationsTests,
        unionEliminationsWithDefaultsTests,
        nestedUnionEliminationsTests,
        unionEliminationsInComplexContextsTests,
        multiParameterPolymorphicCaseStatementsTests,
        higherOrderUnionEliminationsTests,
        recursiveUnionEliminationsTests],
      Testing.testGroupCases = []}

unionEliminationsWithDataTests :: Testing.TestGroup
unionEliminationsWithDataTests =
    Testing.TestGroup {
      Testing.testGroupName = "Union eliminations with data",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Number extracting int values",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeNumberName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "i"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "i"))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeNumberName),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Number converting to string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeNumberName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "i"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showFloat32")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeNumberName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Number applied to int variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = TestTypes.testTypeNumberName,
                Core.caseStatementDefault = Nothing,
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "int"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "i"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "i"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "float"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "f"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))),
              Core.applicationArgument = (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Timestamp with mixed data types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeTimestampName,
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "unixTimeMillis"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "millis"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showUint64")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "millis"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "date"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "dateStr"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "dateStr"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeTimestampName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unionEliminationsWithDefaultsTests :: Testing.TestGroup
unionEliminationsWithDefaultsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Union eliminations with defaults",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Comparison with default case",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeComparisonName,
              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "unknown"))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "lessThan"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "less"))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "equalTo"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "equal"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeComparisonName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match Number with default case",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeNumberName,
              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "i"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "i"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeNumberName),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "match UnionMonomorphic with default",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = TestTypes.testTypeUnionMonomorphicName,
              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "fallback"))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "bool"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showBoolean")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))})))},
                Core.Field {
                  Core.fieldName = (Core.Name "string"),
                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "s"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "s"))})))}]})))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeUnionMonomorphicName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unionInjectionsWithDataTests :: Testing.TestGroup
unionInjectionsWithDataTests =
    Testing.TestGroup {
      Testing.testGroupName = "Union injections with data",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Number int variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeNumberName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "int"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeNumberName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Number float variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeNumberName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "float"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeNumberName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Timestamp unixTimeMillis variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeTimestampName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "unixTimeMillis"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1609459200000)))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeTimestampName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "inject into Timestamp date variant",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeTimestampName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "date"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "2021-01-01"))}})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeTimestampName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unionsInComplexContextsTests :: Testing.TestGroup
unionsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Unions in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "union in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = TestTypes.testTypeNumberName,
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "int"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}), (Core.TermLiteral (Core.LiteralString "context")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable TestTypes.testTypeNumberName),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "union in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "int"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}}),
              (Core.TermUnion (Core.Injection {
                Core.injectionTypeName = TestTypes.testTypeNumberName,
                Core.injectionField = Core.Field {
                  Core.fieldName = (Core.Name "float"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5)))}}))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeVariable TestTypes.testTypeNumberName)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic union in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "value"),
                  Core.bindingTerm = (Core.TermUnion (Core.Injection {
                    Core.injectionTypeName = TestTypes.testTypePersonOrSomethingName,
                    Core.injectionField = Core.Field {
                      Core.fieldName = (Core.Name "other"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "test"))}})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "value"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeApplication (Core.ApplicationType {
              Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypePersonOrSomethingName),
              Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unionsTests :: Testing.TestGroup
unionsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Unions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        simpleUnionInjectionsTests,
        unionInjectionsWithDataTests,
        polymorphicUnionInjectionsTests,
        polymorphicRecursiveUnionInjectionsTests,
        polymorphicUnionsFromLambdaTests,
        unionsInComplexContextsTests,
        multiParameterPolymorphicInjectionsTests],
      Testing.testGroupCases = []}

unwrapEliminationsInApplicationsTests :: Testing.TestGroup
unwrapEliminationsInApplicationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Unwrap eliminations in applications",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap applied to wrapped term",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "hello"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap polymorphic applied",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypePolymorphicWrapperName))),
              Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypePolymorphicWrapperName,
                Core.wrappedTermBody = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

unwrapInComplexContextsTests :: Testing.TestGroup
unwrapInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Unwrap in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "unwrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapped"),
                  Core.bindingTerm = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "test"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "unwrapper")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "wrapped"))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName)), (Core.TermLiteral (Core.LiteralString "context")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeStringAliasName),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unwrap in lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "wrapped"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestTypes.testTypeStringAliasName))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "wrapped"))}))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable TestTypes.testTypeStringAliasName),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

usingKernelTypesTests :: Testing.TestGroup
usingKernelTypesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Using kernel types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case statement on CoderDirection applied to argument",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "dir"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "coder"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "hydra.coders.CoderDirection"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "encode"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "_"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v12"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
                                  Core.projectionField = (Core.Name "encode")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "coder"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v12"))}))})))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "decode"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "_"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v12"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                                  Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
                                  Core.projectionField = (Core.Name "decode")})))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "coder"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v12"))}))})))})))}]})))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "dir"))}))})))}))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.coders.CoderDirection")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                      Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.coders.Coder")),
                      Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.context.Context")),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable (Core.Name "hydra.context.InContext")),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "hydra.errors.Error"))})),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabled"]}]}

usingUnionPolymorphicRecursiveTests :: Testing.TestGroup
usingUnionPolymorphicRecursiveTests =
    Testing.TestGroup {
      Testing.testGroupName = "using UnionPolymorphicRecursive",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-applied UnionPolymorphicRecursive",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "test"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                    Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "value"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "i"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "test"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "applied UnionPolymorphicRecursive with int32",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "test"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                      Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "value"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "i"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                    Core.applicationArgument = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "value"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "test"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "applied UnionPolymorphicRecursive with int32 in lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "test"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                        Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "value"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "i"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "i"))}))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "test"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "applied generic UnionPolymorphicRecursive in lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "test"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = TestTypes.testTypeUnionPolymorphicRecursiveName,
                        Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "other"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "value"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "ignored"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "test"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeUnionPolymorphicRecursiveName),
                  Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))})),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

wrapEliminationsTests :: Testing.TestGroup
wrapEliminationsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Wrap eliminations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        monomorphicUnwrappingTests,
        polymorphicUnwrappingTests,
        unwrapEliminationsInApplicationsTests,
        unwrapInComplexContextsTests,
        multiParameterPolymorphicUnwrappersTests,
        chainedUnwrappingTests,
        multipleUnwrapOperationsTests],
      Testing.testGroupCases = []}

wrappedTermsInComplexContextsTests :: Testing.TestGroup
wrappedTermsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Wrapped terms in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped in record",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermRecord (Core.Record {
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
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypePersonName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "alias"),
                  Core.bindingTerm = (Core.TermWrap (Core.WrappedTerm {
                    Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                    Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "test"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "alias"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeVariable TestTypes.testTypeStringAliasName))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "wrapped in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "first"))}),
              (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = TestTypes.testTypeStringAliasName,
                Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "second"))}))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeVariable TestTypes.testTypeStringAliasName)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}

wrappedTermsTests :: Testing.TestGroup
wrappedTermsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Wrapped terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        monomorphicWrappedTermsTests,
        polymorphicWrappedTermsTests,
        wrappedTermsInComplexContextsTests,
        nestedWrappedTermsTests,
        multipleWrappingLevelsTests,
        multiParameterPolymorphicWrappersTests],
      Testing.testGroupCases = []}
