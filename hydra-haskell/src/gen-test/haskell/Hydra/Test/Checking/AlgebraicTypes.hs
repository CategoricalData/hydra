-- Note: this is an automatically generated file. Do not edit.

-- | Algebraic type checking test cases: unit, pairs, eithers, optionals

module Hydra.Test.Checking.AlgebraicTypes where

import qualified Hydra.Core as Core
import qualified Hydra.Test.TestTypes as TestTypes
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Algebraic types",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    unitTests,
    pairsTests,
    eithersTests,
    optionalsTests],
  Testing.testGroupCases = []}

unitTests :: Testing.TestGroup
unitTests = Testing.TestGroup {
  Testing.testGroupName = "Unit",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    unitTermTests,
    unitTermInPolymorphicContextTests],
  Testing.testGroupCases = []}

unitTermTests :: Testing.TestGroup
unitTermTests = Testing.TestGroup {
  Testing.testGroupName = "Unit term",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "unit literal",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = Core.TermUnit,
        Testing.typeCheckingTestCaseOutputTerm = Core.TermUnit,
        Testing.typeCheckingTestCaseOutputType = Core.TypeUnit})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

unitTermInPolymorphicContextTests :: Testing.TestGroup
unitTermInPolymorphicContextTests = Testing.TestGroup {
  Testing.testGroupName = "Unit term in polymorphic context",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "unit from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = Core.TermUnit}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = Core.TermUnit})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = Core.TypeUnit}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

pairsTests :: Testing.TestGroup
pairsTests = Testing.TestGroup {
  Testing.testGroupName = "Pairs",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    basicPairsTests,
    polymorphicPairsTests,
    pairsInComplexContextsTests,
    nestedPairsTests,
    pairsWithComplexTypesTests],
  Testing.testGroupCases = []}

basicPairsTests :: Testing.TestGroup
basicPairsTests = Testing.TestGroup {
  Testing.testGroupName = "Basic pairs",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair of int and string",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "hello")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "hello")))),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair of string and boolean",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "test"), (Core.TermLiteral (Core.LiteralBoolean True)))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralString "test"), (Core.TermLiteral (Core.LiteralBoolean True)))),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair of boolean and int",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean False), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean False), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100))))),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
          Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeBoolean),
          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

polymorphicPairsTests :: Testing.TestGroup
polymorphicPairsTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphic pairs",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair from lambda (first element)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralString "constant"))))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralString "constant")))),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair from lambda (second element)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermPair (Core.TermLiteral (Core.LiteralString "constant"), (Core.TermVariable (Core.Name "x"))))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralString "constant"), (Core.TermVariable (Core.Name "x")))),
                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t0"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair from two lambdas",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "y"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y")))),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))}))})))})))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair with repeated variable",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x"))))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x")))),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t0"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

pairsInComplexContextsTests :: Testing.TestGroup
pairsInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Pairs in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair in list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one"))),
          (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "two"))))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one")))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}),
          (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "two")))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair in let binding",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "result"),
              Core.bindingTerm = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "answer")))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "result"))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "result"),
              Core.bindingTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "answer")))),
                  Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypePair (Core.PairType {
                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))}))}],
          Core.letBody = (Core.TermVariable (Core.Name "result"))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

nestedPairsTests :: Testing.TestGroup
nestedPairsTests = Testing.TestGroup {
  Testing.testGroupName = "Nested pairs",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair of pairs",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one"))), (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "one")))),
                Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}), (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralBoolean True), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))),
                Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
            Core.typeApplicationTermType = (Core.TypePair (Core.PairType {
              Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Core.typeApplicationTermType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})),
          Core.pairTypeSecond = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair with list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermLiteral (Core.LiteralString "numbers")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermLiteral (Core.LiteralString "numbers")))),
            Core.typeApplicationTermType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list of pairs",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "a"))),
          (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "b"))))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "a")))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}),
          (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "b")))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

pairsWithComplexTypesTests :: Testing.TestGroup
pairsWithComplexTypesTests = Testing.TestGroup {
  Testing.testGroupName = "Pairs with complex types",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair with record on first",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermRecord (Core.Record {
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
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermRecord (Core.Record {
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
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]}), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "Person"))})),
          Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeVariable (Core.Name "Person")),
          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "pair with record on second",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "name"), (Core.TermRecord (Core.Record {
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
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermPair (Core.TermLiteral (Core.LiteralString "name"), (Core.TermRecord (Core.Record {
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
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
          Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "Person"))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypePair (Core.PairType {
          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "Person"))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

eithersTests :: Testing.TestGroup
eithersTests = Testing.TestGroup {
  Testing.testGroupName = "Eithers",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    leftValuesTests,
    rightValuesTests,
    polymorphicEithersTests,
    eithersInComplexContextsTests,
    nestedEithersTests,
    eithersWithComplexTypesTests],
  Testing.testGroupCases = []}

leftValuesTests :: Testing.TestGroup
leftValuesTests = Testing.TestGroup {
  Testing.testGroupName = "Left values",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "left int",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
              Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "left string",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error")))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "left boolean",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralBoolean False)))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralBoolean False)))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

rightValuesTests :: Testing.TestGroup
rightValuesTests = Testing.TestGroup {
  Testing.testGroupName = "Right values",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "right int",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
            Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "right string",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "success")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "success")))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
            Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "right boolean",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralBoolean True)))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralBoolean True)))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
            Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

polymorphicEithersTests :: Testing.TestGroup
polymorphicEithersTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphic eithers",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "left from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "x"))))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "x")))),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))}))})))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t1"))}))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "right from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "x"))))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "x")))),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t1")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either from two lambdas",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "flag"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "flag"))})),
                Core.applicationArgument = (Core.TermEither (Left (Core.TermVariable (Core.Name "x"))))})),
              Core.applicationArgument = (Core.TermEither (Right (Core.TermVariable (Core.Name "x"))))}))})))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "flag"),
            Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeBoolean)),
            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                      Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                        Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                        Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "flag"))})),
                  Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermVariable (Core.Name "x")))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermVariable (Core.Name "x")))),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))}))})))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

eithersInComplexContextsTests :: Testing.TestGroup
eithersInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Eithers in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either in list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error"))),
          (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "error")))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}),
          (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
          Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either in let binding",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "result"),
              Core.bindingTerm = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "result"))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermLet (Core.Let {
            Core.letBindings = [
              Core.Binding {
                Core.bindingName = (Core.Name "result"),
                Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                  Core.typeLambdaParameter = (Core.Name "t1"),
                  Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                      Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))})),
                    Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                Core.bindingType = (Just (Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t1"],
                  Core.typeSchemeType = (Core.TypeEither (Core.EitherType {
                    Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t1")),
                    Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}],
            Core.letBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermVariable (Core.Name "result")),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
            Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

nestedEithersTests :: Testing.TestGroup
nestedEithersTests = Testing.TestGroup {
  Testing.testGroupName = "Nested eithers",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either of either (left left)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
                    Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})))),
                Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))})),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))})),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t1"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either of either (left right)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "nested")))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralString "nested")))),
                    Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Core.typeApplicationTermType = (Core.TypeEither (Core.EitherType {
                  Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                  Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))})),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))}))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
                Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t1"))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either of either (right)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralBoolean True)))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralBoolean True)))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
            Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either of list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))),
              Core.typeApplicationTermType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list of eithers",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "a"))),
          Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))),
          (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "b"))))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "a")))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}),
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}),
          (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermLiteral (Core.LiteralString "b")))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeEither (Core.EitherType {
          Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
          Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

eithersWithComplexTypesTests :: Testing.TestGroup
eithersWithComplexTypesTests = Testing.TestGroup {
  Testing.testGroupName = "Eithers with complex types",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either with record on left",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Left (Core.TermRecord (Core.Record {
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
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Left (Core.TermRecord (Core.Record {
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))}]})))),
              Core.typeApplicationTermType = (Core.TypeVariable TestTypes.testTypePersonName)})),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable TestTypes.testTypePersonName),
            Core.eitherTypeRight = (Core.TypeVariable (Core.Name "t0"))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "either with record on right",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermEither (Right (Core.TermRecord (Core.Record {
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
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermEither (Right (Core.TermRecord (Core.Record {
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
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))}]})))),
              Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))})),
            Core.typeApplicationTermType = (Core.TypeVariable TestTypes.testTypePersonName)}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeEither (Core.EitherType {
            Core.eitherTypeLeft = (Core.TypeVariable (Core.Name "t0")),
            Core.eitherTypeRight = (Core.TypeVariable TestTypes.testTypePersonName)}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

optionalsTests :: Testing.TestGroup
optionalsTests = Testing.TestGroup {
  Testing.testGroupName = "Optionals",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    monomorphicOptionalsTests,
    polymorphicOptionalsTests,
    optionalsInComplexContextsTests,
    nestedOptionalsTests,
    optionalsWithComplexTypesTests],
  Testing.testGroupCases = []}

monomorphicOptionalsTests :: Testing.TestGroup
monomorphicOptionalsTests = Testing.TestGroup {
  Testing.testGroupName = "Monomorphic optionals",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "nothing",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe Nothing),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermMaybe Nothing),
            Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "just int",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "just string",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "just boolean",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralBoolean True)))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralBoolean True)))),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeBoolean))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

polymorphicOptionalsTests :: Testing.TestGroup
polymorphicOptionalsTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphic optionals",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "optional from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "nothing from lambda",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermMaybe Nothing)}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
            Core.typeLambdaParameter = (Core.Name "t1"),
            Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
              Core.lambdaBody = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                Core.typeApplicationTermBody = (Core.TermMaybe Nothing),
                Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t1"))}))})))}))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
            Core.forallTypeParameter = (Core.Name "t1"),
            Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t1")))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "conditional optional",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "flag"),
            Core.lambdaDomain = Nothing,
            Core.lambdaBody = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "flag"))})),
                Core.applicationArgument = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))})),
              Core.applicationArgument = (Core.TermMaybe Nothing)}))})))}))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeLambda (Core.TypeLambda {
          Core.typeLambdaParameter = (Core.Name "t0"),
          Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
            Core.lambdaParameter = (Core.Name "x"),
            Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "flag"),
              Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeBoolean)),
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                      Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                      Core.typeApplicationTermType = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "flag"))})),
                  Core.applicationArgument = (Core.TermMaybe (Just (Core.TermVariable (Core.Name "x"))))})),
                Core.applicationArgument = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermMaybe Nothing),
                  Core.typeApplicationTermType = (Core.TypeVariable (Core.Name "t0"))}))}))})))})))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeForall (Core.ForallType {
          Core.forallTypeParameter = (Core.Name "t0"),
          Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
              Core.functionTypeCodomain = (Core.TypeMaybe (Core.TypeVariable (Core.Name "t0")))}))}))}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

optionalsInComplexContextsTests :: Testing.TestGroup
optionalsInComplexContextsTests = Testing.TestGroup {
  Testing.testGroupName = "Optionals in complex contexts",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "optional in record",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermRecord (Core.Record {
          Core.recordTypeName = TestTypes.testTypeBuddyListAName,
          Core.recordFields = [
            Core.Field {
              Core.fieldName = (Core.Name "head"),
              Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "first"))},
            Core.Field {
              Core.fieldName = (Core.Name "tail"),
              Core.fieldTerm = (Core.TermMaybe (Just (Core.TermRecord (Core.Record {
                Core.recordTypeName = TestTypes.testTypeBuddyListBName,
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "head"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "second"))},
                  Core.Field {
                    Core.fieldName = (Core.Name "tail"),
                    Core.fieldTerm = (Core.TermMaybe Nothing)}]}))))}]})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
          Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
            Core.recordTypeName = TestTypes.testTypeBuddyListAName,
            Core.recordFields = [
              Core.Field {
                Core.fieldName = (Core.Name "head"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "first"))},
              Core.Field {
                Core.fieldName = (Core.Name "tail"),
                Core.fieldTerm = (Core.TermMaybe (Just (Core.TermTypeApplication (Core.TypeApplicationTerm {
                  Core.typeApplicationTermBody = (Core.TermRecord (Core.Record {
                    Core.recordTypeName = TestTypes.testTypeBuddyListBName,
                    Core.recordFields = [
                      Core.Field {
                        Core.fieldName = (Core.Name "head"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "second"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "tail"),
                        Core.fieldTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                          Core.typeApplicationTermBody = (Core.TermMaybe Nothing),
                          Core.typeApplicationTermType = (Core.TypeApplication (Core.ApplicationType {
                            Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
                            Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)}))}))}]})),
                  Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))))}]})),
          Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = (Core.TypeVariable TestTypes.testTypeBuddyListAName),
          Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)}))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "optional in let binding",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "maybeValue"),
              Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "maybeValue"))})),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "maybeValue"),
              Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
              Core.bindingType = (Just (Core.TypeScheme {
                Core.typeSchemeVariables = [],
                Core.typeSchemeType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}))}],
          Core.letBody = (Core.TermVariable (Core.Name "maybeValue"))})),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

nestedOptionalsTests :: Testing.TestGroup
nestedOptionalsTests = Testing.TestGroup {
  Testing.testGroupName = "Nested optionals",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "optional of optional",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "nested")))))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMaybe (Just (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "nested")))))),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "optional of list",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe (Just (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMaybe (Just (Core.TermList [
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "list of optionals",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermList [
          Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "a"))),
          Core.TermMaybe Nothing,
          (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "b"))))]),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermList [
          Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "a"))),
          Core.TermTypeApplication (Core.TypeApplicationTerm {
            Core.typeApplicationTermBody = (Core.TermMaybe Nothing),
            Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}),
          (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "b"))))]),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeList (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

optionalsWithComplexTypesTests :: Testing.TestGroup
optionalsWithComplexTypesTests = Testing.TestGroup {
  Testing.testGroupName = "Optionals with complex types",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "optional map",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseTypeChecking (Testing.TypeCheckingTestCase {
        Testing.typeCheckingTestCaseInput = (Core.TermMaybe (Just (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "key"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])))),
        Testing.typeCheckingTestCaseOutputTerm = (Core.TermMaybe (Just (Core.TermMap (M.fromList [
          (Core.TermLiteral (Core.LiteralString "key"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))])))),
        Testing.typeCheckingTestCaseOutputType = (Core.TypeMaybe (Core.TypeMap (Core.MapType {
          Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
          Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}
