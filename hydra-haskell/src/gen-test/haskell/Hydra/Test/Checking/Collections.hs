-- Note: this is an automatically generated file. Do not edit.

-- | Collection type checking test cases: lists, sets, maps

module Hydra.Test.Checking.Collections where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Collections",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    listsTests,
    setsTests,
    mapsTests],
  Testing.testGroupCases = []}

listsTests :: Testing.TestGroup
listsTests = Testing.TestGroup {
  Testing.testGroupName = "Lists",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    listsOfLiteralsTests,
    emptyListsTests,
    polymorphicListsTests,
    nestedListsTests,
    listsInComplexContextsTests],
  Testing.testGroupCases = []}

listsOfLiteralsTests :: Testing.TestGroup
listsOfLiteralsTests = Testing.TestGroup {
  Testing.testGroupName = "Lists of literals",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "int list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "string list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermLiteral (Core.LiteralString "hello"),
          (Core.TermLiteral (Core.LiteralString "world"))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermLiteral (Core.LiteralString "hello"),
          (Core.TermLiteral (Core.LiteralString "world"))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "single element list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "mixed numeric types",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.0)),
          (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5))),
          (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 1.0)),
          (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 2.5))),
          (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 3.14)))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

emptyListsTests :: Testing.TestGroup
emptyListsTests = Testing.TestGroup {
  Testing.testGroupName = "Empty lists",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "empty list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList []),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermList []),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair of empty lists",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermList [], (Core.TermList []))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermList []),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}), (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermList []),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})))),
                Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
              Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
              Core.pairTypeSecond = (Core.TypeList (Core.TypeVariable (Core.Name "t1")))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "empty list in tuple",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermList [], (Core.TermLiteral (Core.LiteralString "context")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermList []),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}), (Core.TermLiteral (Core.LiteralString "context")))),
              Core.typeApplicationTermType = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
            Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

polymorphicListsTests :: Testing.TestGroup
polymorphicListsTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphic lists",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermList [
            Core.TermVariable (Core.Name "x")])}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermList [
              Core.TermVariable (Core.Name "x")])})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list with repeated var",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermList [
            Core.TermVariable (Core.Name "x"),
            (Core.TermVariable (Core.Name "x"))])}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermList [
              Core.TermVariable (Core.Name "x"),
              (Core.TermVariable (Core.Name "x"))])})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list from two lambdas",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "y"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermList [
              Core.TermVariable (Core.Name "x"),
              (Core.TermVariable (Core.Name "y"))])})))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                (Core.TermVariable (Core.Name "y"))])})))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

nestedListsTests :: Testing.TestGroup
nestedListsTests = Testing.TestGroup {
  Testing.testGroupName = "Nested lists",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list of lists",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermList [
            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
          (Core.TermList [
            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermList [
            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
          (Core.TermList [
            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "empty nested lists",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermList [],
          (Core.TermList [])]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermList [
            Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermList []),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}),
            (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermList []),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))])})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "nested polymorphic",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermList [
            Core.TermList [
              Core.TermVariable (Core.Name "x")]])}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermList [
              Core.TermList [
                Core.TermVariable (Core.Name "x")]])})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

listsInComplexContextsTests :: Testing.TestGroup
listsInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Lists in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "multiple lists in tuple",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermList [
          Core.TermLiteral (Core.LiteralString "a"),
          (Core.TermLiteral (Core.LiteralString "b"))]))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))]))),
            Core.typeApplicationTermType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Core.typeApplicationTermType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
          Core.pairTypeSecond = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

mapsTests :: Testing.TestGroup
mapsTests = Testing.TestGroup {
  Testing.testGroupName = "Maps",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    monomorphicMapsTests,
    polymorphicMapsTests,
    mapsInComplexContextsTests,
    mapsWithComplexTypesTests],
  Testing.testGroupCases = []}

monomorphicMapsTests :: Testing.TestGroup
monomorphicMapsTests = Testing.TestGroup {
  Testing.testGroupName = "Monomorphic maps",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "empty map",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap M.empty),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermMap M.empty),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "int to string map",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one"))),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "two")))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one"))),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "two")))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "string to int map",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
          (Core.TermLiteral (Core.LiteralString "b"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "a"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
          (Core.TermLiteral (Core.LiteralString "b"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "single entry map",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)), (Core.TermLiteral (Core.LiteralBoolean True)))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueBigint 42)), (Core.TermLiteral (Core.LiteralBoolean True)))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)),
          Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

polymorphicMapsTests :: Testing.TestGroup
polymorphicMapsTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphic maps",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map from lambda keys",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "k"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermMap (M.fromList [
            (Core.TermVariable (Core.Name "k"), (Core.TermLiteral (Core.LiteralString "value")))]))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "k"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "k"), (Core.TermLiteral (Core.LiteralString "value")))]))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
              Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map from lambda values",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "v"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermMap (M.fromList [
            (Core.TermLiteral (Core.LiteralString "key"), (Core.TermVariable (Core.Name "v")))]))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralString "key"), (Core.TermVariable (Core.Name "v")))]))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map from lambda both",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "k"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "v"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "k"), (Core.TermVariable (Core.Name "v")))]))})))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "k"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "v"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                Core.lambdaBody = (Core.TermMap (M.fromList [
                  (Core.TermVariable (Core.Name "k"), (Core.TermVariable (Core.Name "v")))]))})))})))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                  Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map with repeated variables",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermMap (M.fromList [
            (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x")))]))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermMap (M.fromList [
              (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x")))]))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
              Core.mapTypeValues = (Core.TypeVariable (Core.Name "t0"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

mapsInComplexContextsTests :: Testing.TestGroup
mapsInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Maps in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map in tuple",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one")))]), (Core.TermLiteral (Core.LiteralString "context")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermMap (M.fromList [
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one")))]), (Core.TermLiteral (Core.LiteralString "context")))),
            Core.typeApplicationTermType = (Core.TypeMap (Core.MapType {
              Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)})),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "nested maps",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "outer"), (Core.TermMap (M.fromList [
            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralBoolean True)))])))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "outer"), (Core.TermMap (M.fromList [
            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralBoolean True)))])))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypeMap (Core.MapType {
            Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map in let binding",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "lookup"),
              Core.bindingTerm = (Core.TermMap (M.fromList [
                (Core.TermLiteral (Core.LiteralString "key1"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))),
                (Core.TermLiteral (Core.LiteralString "key2"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200))))])),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "lookup"))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "lookup"),
              Core.bindingTerm = (Core.TermMap (M.fromList [
                (Core.TermLiteral (Core.LiteralString "key1"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))),
                (Core.TermLiteral (Core.LiteralString "key2"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 200))))])),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                  Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.typeSchemeConstraints = Nothing}))}],
          Core.letBody = (Core.TermVariable (Core.Name "lookup"))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

mapsWithComplexTypesTests :: Testing.TestGroup
mapsWithComplexTypesTests = Testing.TestGroup {
  Testing.testGroupName = "Maps with complex types",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map of records",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
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
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
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
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypeVariable TestTypes.testTypePersonName)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map of lists",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermList [
            Core.TermLiteral (Core.LiteralString "a"),
            (Core.TermLiteral (Core.LiteralString "b"))])),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermList [
            Core.TermLiteral (Core.LiteralString "c"),
            (Core.TermLiteral (Core.LiteralString "d"))]))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermList [
            Core.TermLiteral (Core.LiteralString "a"),
            (Core.TermLiteral (Core.LiteralString "b"))])),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermList [
            Core.TermLiteral (Core.LiteralString "c"),
            (Core.TermLiteral (Core.LiteralString "d"))]))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.mapTypeValues = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "map of tuples",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "coords"), (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))))))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "coords"), (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

setsTests :: Testing.TestGroup
setsTests = Testing.TestGroup {
  Testing.testGroupName = "Sets",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    monomorphicSetsTests,
    polymorphicSetsTests,
    setsInComplexContextsTests,
    nestedSetsTests,
    setsWithComplexTypesTests],
  Testing.testGroupCases = []}

monomorphicSetsTests :: Testing.TestGroup
monomorphicSetsTests = Testing.TestGroup {
  Testing.testGroupName = "Monomorphic sets",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "empty set",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet S.empty),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermSet S.empty),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "int set",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "string set",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralString "apple"),
          (Core.TermLiteral (Core.LiteralString "banana")),
          (Core.TermLiteral (Core.LiteralString "cherry"))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralString "apple"),
          (Core.TermLiteral (Core.LiteralString "banana")),
          (Core.TermLiteral (Core.LiteralString "cherry"))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "single element set",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralBoolean True)])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralBoolean True)])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeBoolean))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

polymorphicSetsTests :: Testing.TestGroup
polymorphicSetsTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphic sets",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermSet (S.fromList [
            Core.TermVariable (Core.Name "x")]))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermSet (S.fromList [
              Core.TermVariable (Core.Name "x")]))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set with repeated variable",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermSet (S.fromList [
            Core.TermVariable (Core.Name "x")]))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermSet (S.fromList [
              Core.TermVariable (Core.Name "x")]))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set from two variables",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "y"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermSet (S.fromList [
              Core.TermVariable (Core.Name "x"),
              (Core.TermVariable (Core.Name "y"))]))})))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermSet (S.fromList [
                Core.TermVariable (Core.Name "x"),
                (Core.TermVariable (Core.Name "y"))]))})))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeSet (Core.TypeVariable (Core.Name "t0")))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

setsInComplexContextsTests :: Testing.TestGroup
setsInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Sets in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set in tuple",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermSet (S.fromList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]), (Core.TermLiteral (Core.LiteralString "context")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]), (Core.TermLiteral (Core.LiteralString "context")))),
            Core.typeApplicationTermType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set in let binding",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "numbers"),
              Core.bindingTerm = (Core.TermSet (S.fromList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "numbers"))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "numbers"),
              Core.bindingTerm = (Core.TermSet (S.fromList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                Core.typeSchemeConstraints = Nothing}))}],
          Core.letBody = (Core.TermVariable (Core.Name "numbers"))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

nestedSetsTests :: Testing.TestGroup
nestedSetsTests = Testing.TestGroup {
  Testing.testGroupName = "Nested sets",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set of lists",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermList [
            Core.TermLiteral (Core.LiteralString "a"),
            (Core.TermLiteral (Core.LiteralString "b"))],
          (Core.TermList [
            Core.TermLiteral (Core.LiteralString "c"),
            (Core.TermLiteral (Core.LiteralString "d"))])])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermList [
            Core.TermLiteral (Core.LiteralString "a"),
            (Core.TermLiteral (Core.LiteralString "b"))],
          (Core.TermList [
            Core.TermLiteral (Core.LiteralString "c"),
            (Core.TermLiteral (Core.LiteralString "d"))])])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set of tuples",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))),
          (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))))])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}),
          (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set of sets",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermSet (S.fromList [
            Core.TermLiteral (Core.LiteralString "nested")])])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermSet (S.fromList [
            Core.TermLiteral (Core.LiteralString "nested")])])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeString)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

setsWithComplexTypesTests :: Testing.TestGroup
setsWithComplexTypesTests = Testing.TestGroup {
  Testing.testGroupName = "Sets with complex types",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set of records",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
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
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
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
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeVariable TestTypes.testTypePersonName))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set of optionals",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))),
          (Core.TermMaybe Nothing)])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))),
          (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermMaybe Nothing),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "set of maps",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermSet (S.fromList [
          Core.TermMap (M.fromList [
            (Core.TermLiteral (Core.LiteralString "key"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])])),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermSet (S.fromList [
          Core.TermMap (M.fromList [
            (Core.TermLiteral (Core.LiteralString "key"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])])),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeSet (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}
