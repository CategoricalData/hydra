-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for term reduction/evaluation mechanics

module Hydra.Test.Reduction where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as Core_
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Test cases for term reduction mechanics
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "reduction",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "beta reduction",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "identity function applied to literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "constant function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "monomorphic primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "toUpper on lowercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "HELLO")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "toUpper on mixed case",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Hello World"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "HELLO WORLD")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "toUpper on empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "toLower on uppercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "HELLO"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "hello")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string length",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string length of empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "add two positive integers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "add negative and positive",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-10))))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-7)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "add with zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "subtract integers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiply integers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiply by zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "divide integers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.div"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "modulo",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mod"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 17)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "splitOn basic",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a,b,c"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cat2 strings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "helloworld")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "polymorphic primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "length of integer list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "length of string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b"))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "length of empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "length of single element list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralBoolean True)])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "head of integer list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "head of string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "first"),
                    (Core.TermLiteral (Core.LiteralString "second"))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "first")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "last of integer list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.last"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "concat two integer lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "concat with empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "reverse integer list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "reverse empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "nullary primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty set has size zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.size"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.empty")))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "literals as values",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "integer literal is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative integer literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17)))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-17)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "zero integer literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string literal is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralString "hello")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "hello")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralString "")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string with special characters",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralString "hello\nworld\ttab")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralString "hello\nworld\ttab")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "boolean true is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralBoolean True)))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "boolean false is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralBoolean False)))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "float literal is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative float literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.718)))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 (-2.718)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "zero float literal",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "list reduction",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermList []))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list of literals is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list with reducible element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermList [
                  Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "optional reduction",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nothing is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermMaybe Nothing))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just literal is a value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "just with reducible content",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> Core_.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermMaybe (Just (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "alpha conversion",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable at top level",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermVariable (Core.Name "x")))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermVariable (Core.Name "y")))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "variable in list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                  (Core.TermVariable (Core.Name "x"))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                  (Core.TermVariable (Core.Name "y"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda with different variable is transparent",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                    (Core.TermVariable (Core.Name "x")),
                    (Core.TermVariable (Core.Name "z"))])}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                    (Core.TermVariable (Core.Name "y")),
                    (Core.TermVariable (Core.Name "z"))])}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda with same variable is opaque",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                    (Core.TermVariable (Core.Name "x")),
                    (Core.TermVariable (Core.Name "z"))])}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                    (Core.TermVariable (Core.Name "x")),
                    (Core.TermVariable (Core.Name "z"))])}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lambda outer variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "a"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "a"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested lambda shadows outer",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "z") (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "application with variable",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "application with both variables same",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Core_.term (Reduction.alphaConvert (Core.Name "x") (Core.Name "y") (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "type reduction",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unit type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph Core.TypeUnit)),
                Testing.universalTestCaseExpected = (Core_.type_ Core.TypeUnit)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeLiteral Core.LiteralTypeString))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral Core.LiteralTypeString))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "int32 type unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "identity type applied to string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "t"),
                    Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))}))})),
                  Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "constant type ignores argument",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "x"),
                    Core.forallTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeBoolean)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested forall first application",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "x"),
                    Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                      Core.forallTypeParameter = (Core.Name "y"),
                      Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))}))})),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.Name "y"),
                  Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "nested forall both applications",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeApplication (Core.ApplicationType {
                    Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                      Core.forallTypeParameter = (Core.Name "x"),
                      Core.forallTypeBody = (Core.TypeForall (Core.ForallType {
                        Core.forallTypeParameter = (Core.Name "y"),
                        Core.forallTypeBody = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "x")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "y"))}))}))})),
                    Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                  Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list type applied",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "a"),
                    Core.forallTypeBody = (Core.TypeList (Core.TypeVariable (Core.Name "a")))})),
                  Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "optional type applied",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<type reduction error>>") (\t -> Core_.type_ t) (Reduction.betaReduceType TestGraph.testContext TestGraph.testGraph (Core.TypeApplication (Core.ApplicationType {
                  Core.applicationTypeFunction = (Core.TypeForall (Core.ForallType {
                    Core.forallTypeParameter = (Core.Name "a"),
                    Core.forallTypeBody = (Core.TypeMaybe (Core.TypeVariable (Core.Name "a")))})),
                  Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)})))),
                Testing.universalTestCaseExpected = (Core_.type_ (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "etaExpandTerm",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "integer literal unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string list unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "bar"))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo"),
                  (Core.TermLiteral (Core.LiteralString "bar"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fully applied binary function unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda with fully applied primitive unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda returning constant unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare unary primitive unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "bare binary primitive unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partially applied binary primitive expands to one lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "foo"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "projection expands to lambda",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = (Core.Name "Person"),
                  Core.projectionField = (Core.Name "firstName")})))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v1"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = (Core.Name "Person"),
                      Core.projectionField = (Core.Name "firstName")})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partial application inside lambda expands",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))}))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with constant body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "let with bare primitive value unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "foo"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "foo"),
                      Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "foo"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "fully applied unary unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))})))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partial application in list expands",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\_ -> "eta expansion failed") (\t -> Core_.term t) (Reduction.etaExpandTypedTerm TestGraph.testContext TestGraph.testGraph (Core.TermList [
                  Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo")])})),
                  (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))]))),
                Testing.universalTestCaseExpected = (Core_.term (Core.TermList [
                  Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo")])})),
                  (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "v1"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
