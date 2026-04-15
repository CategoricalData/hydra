-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hydra.lib.logic primitives

module Hydra.Test.Lib.Logic where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Test cases for hydra.lib.logic primitives
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hydra.lib.logic primitives",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "and",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true and true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.and")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true and false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.and")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false and true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.and")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false and false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.and")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "ifElse",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "boolean values",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "true condition returns then",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "false condition returns else",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "integer values",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "true selects first int",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "false selects second int",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "string values",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "true selects first string",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "yes"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "no"))})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "yes")))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "false selects second string",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                    Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "yes"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "no"))})))),
                    Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralString "no")))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "not",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "not true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.not")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "not false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.not")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "or",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true or true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.or")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "true or false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.or")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false or true",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.or")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean True)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "false or false",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<eval error>>") (\t -> ShowCore.term t) (Reduction.reduceTerm TestGraph.testContext TestGraph.testGraph True (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.or")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLiteral (Core.LiteralBoolean False)))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
