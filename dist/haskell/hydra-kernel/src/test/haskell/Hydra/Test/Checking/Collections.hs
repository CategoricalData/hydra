-- Note: this is an automatically generated file. Do not edit.
-- | Collection type checking test cases: lists, sets, maps

module Hydra.Test.Checking.Collections where
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
import qualified Data.Set as S
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Collections",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        listsTests,
        setsTests,
        mapsTests],
      Testing.testGroupCases = []}
emptyListsTests :: Testing.TestGroup
emptyListsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Empty lists",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList []))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "pair of empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [], (Core.TermList []))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                Core.forallTypeParameter = (Core.Name "t1"),
                Core.forallTypeBody = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                  Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [], (Core.TermLiteral (Core.LiteralString "context")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
listsInComplexContextsTests :: Testing.TestGroup
listsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Lists in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple lists in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))]))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.pairTypeSecond = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
listsOfLiteralsTests :: Testing.TestGroup
listsOfLiteralsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Lists of literals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello"),
              (Core.TermLiteral (Core.LiteralString "world"))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed numeric types",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.0)),
              (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5))),
              (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
listsTests :: Testing.TestGroup
listsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Lists",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        listsOfLiteralsTests,
        emptyListsTests,
        polymorphicListsTests,
        nestedListsTests,
        listsInComplexContextsTests],
      Testing.testGroupCases = []}
mapsInComplexContextsTests :: Testing.TestGroup
mapsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Maps in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one")))]), (Core.TermLiteral (Core.LiteralString "context")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested maps",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralString "outer"), (Core.TermMap (M.fromList [
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralBoolean True)))])))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeMap (Core.MapType {
                Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "lookup"),
                  Core.bindingTerm = (Core.TermMap (M.fromList [
                    (Core.TermLiteral (Core.LiteralString "key1"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))),
                    (Core.TermLiteral (Core.LiteralString "key2"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200))))])),
                  Core.bindingTypeScheme = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "lookup"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
mapsTests :: Testing.TestGroup
mapsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Maps",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        monomorphicMapsTests,
        polymorphicMapsTests,
        mapsInComplexContextsTests,
        mapsWithComplexTypesTests],
      Testing.testGroupCases = []}
mapsWithComplexTypesTests :: Testing.TestGroup
mapsWithComplexTypesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Maps with complex types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map of records",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralString "person1"), (Core.TermRecord (Core.Record {
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeVariable TestTypes.testTypePersonName)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map of lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermList [
                Core.TermLiteral (Core.LiteralString "c"),
                (Core.TermLiteral (Core.LiteralString "d"))]))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.mapTypeValues = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map of tuples",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralString "coords"), (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))))))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypePair (Core.PairType {
                Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
monomorphicMapsTests :: Testing.TestGroup
monomorphicMapsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic maps",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap M.empty))),
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
          Testing.testCaseWithMetadataName = "int to string map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one"))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "two")))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string to int map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
              (Core.TermLiteral (Core.LiteralString "b"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single entry map",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)), (Core.TermLiteral (Core.LiteralBoolean True)))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)),
              Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeBoolean)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
monomorphicSetsTests :: Testing.TestGroup
monomorphicSetsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Monomorphic sets",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet S.empty))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "int set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralString "apple"),
              (Core.TermLiteral (Core.LiteralString "banana")),
              (Core.TermLiteral (Core.LiteralString "cherry"))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralBoolean True)])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeBoolean)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
nestedListsTests :: Testing.TestGroup
nestedListsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested lists",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list of lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty nested lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermList [
              Core.TermList [],
              (Core.TermList [])]))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested polymorphic",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermList [
                  Core.TermVariable (Core.Name "x")]])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
nestedSetsTests :: Testing.TestGroup
nestedSetsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Nested sets",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralString "c"),
                (Core.TermLiteral (Core.LiteralString "d"))])])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of tuples",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))),
              (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))))])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of sets",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermSet (S.fromList [
                Core.TermLiteral (Core.LiteralString "nested")])])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
polymorphicListsTests :: Testing.TestGroup
polymorphicListsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic lists",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list from lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermVariable (Core.Name "x")])})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list with repeated var",
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
          Testing.testCaseWithMetadataName = "list from two lambdas",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermVariable (Core.Name "y"))])}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
polymorphicMapsTests :: Testing.TestGroup
polymorphicMapsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic maps",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map from lambda keys",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "k"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermMap (M.fromList [
                (Core.TermVariable (Core.Name "k"), (Core.TermLiteral (Core.LiteralString "value")))]))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                  Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map from lambda values",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermMap (M.fromList [
                (Core.TermLiteral (Core.LiteralString "key"), (Core.TermVariable (Core.Name "v")))]))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "map from lambda both",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "k"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "v"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermMap (M.fromList [
                  (Core.TermVariable (Core.Name "k"), (Core.TermVariable (Core.Name "v")))]))}))})))),
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
          Testing.testCaseWithMetadataName = "map with repeated variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermMap (M.fromList [
                (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x")))]))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
polymorphicSetsTests :: Testing.TestGroup
polymorphicSetsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic sets",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set from lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermSet (S.fromList [
                Core.TermVariable (Core.Name "x")]))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set with repeated variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermSet (S.fromList [
                Core.TermVariable (Core.Name "x")]))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set from two variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermSet (S.fromList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermVariable (Core.Name "y"))]))}))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeForall (Core.ForallType {
              Core.forallTypeParameter = (Core.Name "t0"),
              Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                  Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
setsInComplexContextsTests :: Testing.TestGroup
setsInComplexContextsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Sets in complex contexts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set in tuple",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermPair (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]), (Core.TermLiteral (Core.LiteralString "context")))))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set in let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "numbers"),
                  Core.bindingTerm = (Core.TermSet (S.fromList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])),
                  Core.bindingTypeScheme = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "numbers"))})))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
setsTests :: Testing.TestGroup
setsTests =
    Testing.TestGroup {
      Testing.testGroupName = "Sets",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        monomorphicSetsTests,
        polymorphicSetsTests,
        setsInComplexContextsTests,
        nestedSetsTests,
        setsWithComplexTypesTests],
      Testing.testGroupCases = []}
setsWithComplexTypesTests :: Testing.TestGroup
setsWithComplexTypesTests =
    Testing.TestGroup {
      Testing.testGroupName = "Sets with complex types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of records",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeVariable TestTypes.testTypePersonName)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of optionals",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))),
              (Core.TermMaybe Nothing)])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "set of maps",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\result -> ShowCore.type_ (Scoping.typeSchemeToFType (Pairs.second (Pairs.first result)))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermSet (S.fromList [
              Core.TermMap (M.fromList [
                (Core.TermLiteral (Core.LiteralString "key"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])])))),
            Testing.universalTestCaseExpected = (ShowCore.type_ (Core.TypeSet (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
