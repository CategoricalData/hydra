-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.lists primitives

module Hydra.Test.Lib.Lists where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Test cases for hydra.lib.lists primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.lists primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "apply",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "string transformations",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "string transformations",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.apply")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermVariable (Core.Name "hydra.lib.strings.toUpper"),
                          (Core.TermVariable (Core.Name "hydra.lib.strings.toLower"))])})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermLiteral (Core.LiteralString "One"),
                        (Core.TermLiteral (Core.LiteralString "Two")),
                        (Core.TermLiteral (Core.LiteralString "Three"))])})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "ONE"),
                      (Core.TermLiteral (Core.LiteralString "TWO")),
                      (Core.TermLiteral (Core.LiteralString "THREE")),
                      (Core.TermLiteral (Core.LiteralString "one")),
                      (Core.TermLiteral (Core.LiteralString "two")),
                      (Core.TermLiteral (Core.LiteralString "three"))]))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "edge cases",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "empty function list",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.apply")),
                        Core.applicationArgument = (Core.TermList [])})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermLiteral (Core.LiteralString "a"),
                        (Core.TermLiteral (Core.LiteralString "b"))])})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "empty input list",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.apply")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermVariable (Core.Name "hydra.lib.strings.toUpper")])})),
                      Core.applicationArgument = (Core.TermList [])})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "single function",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.apply")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermVariable (Core.Name "hydra.lib.strings.toUpper")])})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermLiteral (Core.LiteralString "hello")])})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "HELLO")]))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "single input",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.apply")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermVariable (Core.Name "hydra.lib.strings.toUpper"),
                          (Core.TermVariable (Core.Name "hydra.lib.strings.toLower"))])})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermLiteral (Core.LiteralString "Test")])})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "TEST"),
                      (Core.TermLiteral (Core.LiteralString "test"))]))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "bind",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negation function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.bind")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negate")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-2)))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-3)))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-4))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.bind")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negate")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.bind")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))])})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negate")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicate elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.bind")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.negate")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1)))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-2))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "concat",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple non-empty lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty lists included",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    (Core.TermList []),
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all empty lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [],
                    (Core.TermList []),
                    (Core.TermList [])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list of lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "concat2",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two non-empty lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first list empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "second list empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "both lists empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.concat2")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "a"),
                      (Core.TermLiteral (Core.LiteralString "b"))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "c"),
                    (Core.TermLiteral (Core.LiteralString "d"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c")),
                  (Core.TermLiteral (Core.LiteralString "d"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "cons",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cons to non-empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cons to empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cons negative number",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cons string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.cons")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "world")])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "hello"),
                  (Core.TermLiteral (Core.LiteralString "world"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "drop",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop from beginning",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop zero elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop more than length",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop from empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop negative amount",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "dropWhile",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop while less than 3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.dropWhile")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.dropWhile")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "drop no elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.dropWhile")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.dropWhile")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "elem",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "element present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "element not present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element not present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicate elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string element present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "world"),
                    (Core.TermLiteral (Core.LiteralString "hello")),
                    (Core.TermLiteral (Core.LiteralString "test"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string element not present",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.elem")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "missing"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "world"),
                    (Core.TermLiteral (Core.LiteralString "hello"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "filter",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter positive numbers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-3)))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "filter no elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.filter")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "find",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find existing element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.find")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find first matching",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.find")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find no match",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.find")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find in empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.find")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "find single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.find")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.equal")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "foldl",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sum with addition",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "product with multiplication",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.mul"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 24))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "subtraction fold",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldl")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.sub"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "foldr",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "subtraction fold right",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldr")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.sub"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldr")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldr")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sum with addition",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldr")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "subtraction vs foldl",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.foldr")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.sub"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-8)))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "group",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "consecutive duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.group")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))]),
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.group")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))]),
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all same",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.group")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.group")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.group")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "intercalate",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "double zero separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intercalate")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intercalate")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element separator",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intercalate")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))]),
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list of lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intercalate")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intercalate")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lists with empty lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intercalate")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]),
                    (Core.TermList [])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "intersperse",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string interspersion",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intersperse")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "and"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "one"),
                    (Core.TermLiteral (Core.LiteralString "two")),
                    (Core.TermLiteral (Core.LiteralString "three"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "one"),
                  (Core.TermLiteral (Core.LiteralString "and")),
                  (Core.TermLiteral (Core.LiteralString "two")),
                  (Core.TermLiteral (Core.LiteralString "and")),
                  (Core.TermLiteral (Core.LiteralString "three"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intersperse")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "x"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "only")])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "only")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intersperse")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "x"))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intersperse")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "+"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "+")),
                  (Core.TermLiteral (Core.LiteralString "b"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "number interspersion",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.intersperse")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "length",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.length")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.length")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.length")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "many elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.length")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 9))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.length")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b")),
                    (Core.TermLiteral (Core.LiteralString "c"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "map",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string to uppercase",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.toUpper"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "one"),
                    (Core.TermLiteral (Core.LiteralString "two"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "ONE"),
                  (Core.TermLiteral (Core.LiteralString "TWO"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.toUpper"))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.toUpper"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "hello")])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "HELLO")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "number negation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-2)))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-3))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "identity function",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.map")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.equality.identity"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybeAt",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "valid index",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "last element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "out of bounds",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative index",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeAt")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybeHead",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-empty int list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty int list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-empty string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "hello"),
                    (Core.TermLiteral (Core.LiteralString "world"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "hello")))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybeInit",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeInit")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeInit")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermList []))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeInit")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybeLast",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeLast")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeLast")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeLast")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "maybeTail",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeTail")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeTail")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermList []))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeTail")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "nub",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "remove duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.nub")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.nub")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "all duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.nub")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.nub")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.nub")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.nub")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b")),
                    (Core.TermLiteral (Core.LiteralString "a")),
                    (Core.TermLiteral (Core.LiteralString "c"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "c"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "null",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty int list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.null")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.null")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.null")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.null")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "non-empty string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.null")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a")])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "partition",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partition greater than 3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.partition")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))], (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partition all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.partition")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))], (Core.TermList []))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partition no elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.partition")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [], (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "partition even numbers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.partition")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.even")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))], (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.partition")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [], (Core.TermList []))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "pure",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "one")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "number element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative number",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.pure")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-5)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "replicate",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replicate three times",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.replicate")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replicate zero times",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.replicate")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replicate once",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.replicate")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "replicate string",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.replicate")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "hello"),
                  (Core.TermLiteral (Core.LiteralString "hello"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "reverse",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.reverse")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.reverse")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.reverse")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.reverse")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.reverse")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b")),
                    (Core.TermLiteral (Core.LiteralString "c"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "c"),
                  (Core.TermLiteral (Core.LiteralString "b")),
                  (Core.TermLiteral (Core.LiteralString "a"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "singleton",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "number element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.singleton")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "negative number",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.singleton")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "zero",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.singleton")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.singleton")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "hello")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "sort",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "unsorted numbers",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "already sorted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "reverse sorted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicates",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string sort",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sort")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "zebra"),
                    (Core.TermLiteral (Core.LiteralString "apple")),
                    (Core.TermLiteral (Core.LiteralString "banana"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "apple"),
                  (Core.TermLiteral (Core.LiteralString "banana")),
                  (Core.TermLiteral (Core.LiteralString "zebra"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "sortOn",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sort by string length",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sortOn")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.length"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "hello"),
                    (Core.TermLiteral (Core.LiteralString "hi")),
                    (Core.TermLiteral (Core.LiteralString "world"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "hi"),
                  (Core.TermLiteral (Core.LiteralString "hello")),
                  (Core.TermLiteral (Core.LiteralString "world"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sortOn")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.length"))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single string element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sortOn")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.length"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "test")])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "test")]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sort by negation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sortOn")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.negate"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "sort by absolute value",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.sortOn")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.abs"))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-3)))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-3))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "span",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "span less than 3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.span")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))], (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "span all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.span")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))], (Core.TermList []))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "span no elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.span")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.gt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [], (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.span")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.equality.lt")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermPair (Core.TermList [], (Core.TermList []))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "take",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "take from beginning",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.take")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "take zero elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.take")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "take all elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.take")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "take more than length",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.take")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "take from empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.take")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "take negative amount",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.take")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (-1))))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "transpose",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "square matrix",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.transpose")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.transpose")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single row",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.transpose")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))]),
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single column",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.transpose")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))]),
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "ragged matrix",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.transpose")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))]),
                    (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))],
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                  (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))])]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "uncons",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "three elements",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.uncons")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "single element",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.uncons")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe (Just (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermList []))))))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.uncons")),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermMaybe Nothing))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "zip",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "equal length lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zip")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b")),
                    (Core.TermLiteral (Core.LiteralString "c"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "a"))),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "b")))),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)), (Core.TermLiteral (Core.LiteralString "c"))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first list shorter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zip")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b")),
                    (Core.TermLiteral (Core.LiteralString "c"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "a"))),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "b"))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "second list shorter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zip")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermLiteral (Core.LiteralString "a"))),
                  (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralString "b"))))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty first list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zip")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "a"),
                    (Core.TermLiteral (Core.LiteralString "b"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty second list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zip")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "both empty lists",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zip")),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "zipWith",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "addition",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zipWith")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 9)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "first list shorter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zipWith")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "second list shorter",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zipWith")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty first list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zipWith")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermList [])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty second list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zipWith")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.math.add"))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
                  Core.applicationArgument = (Core.TermList [])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList []))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "string concatenation",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.zipWith")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2"))})),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "a"),
                      (Core.TermLiteral (Core.LiteralString "b"))])})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralString "1"),
                    (Core.TermLiteral (Core.LiteralString "2"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a1"),
                  (Core.TermLiteral (Core.LiteralString "b2"))]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
